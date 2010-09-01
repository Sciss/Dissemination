package de.sciss.semi

import java.net.InetSocketAddress
import de.sciss.synth.io.{AudioFileType, SampleFormat, AudioFileSpec}
import java.util.Properties
import de.sciss.osc.{OSCMessage, TCP, OSCClient}
import java.io.{FileOutputStream, File}
import actors.{TIMEOUT, DaemonActor}
// quickly hacked...
object FScape {
   lazy val client = {
      val res = OSCClient( TCP )
      res.target = new InetSocketAddress( "127.0.0.1", 0x4653 )
      res.action = (msg, addr, when) => FScapeActor ! msg
      res.start
      res
   }

   private case class Process( name: String, doc: Doc, fun: Boolean => Unit )

   private object FScapeActor extends DaemonActor {
      var syncID = -1

      start

      def act = loop { react {
         case Process( name, doc, fun ) => {
            def timedOut( msg: OSCMessage ) {
               println( "FScape : TIMEOUT (" + name + " -- " + msg + ")" )
               fun( false )
            }

            def query( path: String, properties: Seq[ String ], timeOut: Long = 4000L )( handler: Seq[ Any ] => Unit ) {
               syncID += 1
               val sid = syncID
               val msg = OSCMessage( path, ("query" +: syncID +: properties): _* )
               client ! msg
               reactWithin( timeOut ) {
                  case TIMEOUT => timedOut( msg )
                  case OSCMessage( "/query.reply", `sid`, values @ _* ) => handler( values )
               }
            }

            val docFile = File.createTempFile( "semi", ".fsc" ).getAbsolutePath
            val prop    = new Properties()
            prop.setProperty( "Class", "de.sciss.fscape.gui." + doc.className )
            doc.toProperties( prop )
            val os      = new FileOutputStream( docFile )
            prop.store( os, "Dissemination" )
            os.close
            client ! OSCMessage( "/doc", "open", docFile, 1 )    // 0 for invisible
            query( "/doc", "count" :: Nil ) {
               case Seq( num: Int ) => {
                  var idx = 0
                  var found = false
                  loopWhile( !found && (idx < num) ) {
                     query( "/doc/index/" + idx, "id" :: "file" :: Nil ) {
                        case Seq( id, `docFile` ) => {
                           val addr = "/doc/id/" + id
                           found = true
                           client ! OSCMessage( addr, "start" )
                           query( "/main", "version" :: Nil ) { // tricky sync
                              case _ => {
                                 var progress   = 0f
                                 var running    = 1
                                 var err        = ""

                                 loopWhile( running != 0 ) {
                                    reactWithin( 1000L ) {
                                       case TIMEOUT => query( addr, "running" :: "progression" :: "error" :: Nil ) {
                                          case Seq( r: Int, p: Float, e: String ) => {
                                             progress = p
                                             println( "PROGRESS = " + (p * 100).toInt )
                                             running  = r
                                             err      = e
                                          }
                                       }
                                    }
                                 } andThen {
                                    client ! OSCMessage( addr, "close" )
                                    if( err != "" ) {
                                       println( "FScape : ERROR (" + name + " -- " + err + ")" )
                                       fun( false )
                                    } else {
                                       println( "FScape : Success (" + name + ")" )
                                       fun( true )
                                    }
                                 }
                              }
                           }
                        }
                        case _ => idx += 1
                     }
                  } andThen {
                     println( "?! File not found : " + docFile )
                     fun( false )
                  }
               }
            }
         }
         case _ =>
      }}
   }

   def process( name: String, doc: Doc )( fun: Boolean => Unit ) {
      FScapeActor ! Process( name, doc, fun )
   }

   def processChain( name: String, docs: Seq[ Doc ])( fun: Boolean => Unit ) {
      docs.headOption.map( doc => process( name, doc ) { success =>
         if( success ) {
            processChain( name, docs.tail )( fun )
         } else {
            fun( false )
         }
      }).getOrElse( fun( true ))
   }

   object Gain {
      val immediate  = Gain( "0.0dB", false )
      val normalized = Gain( "-0.2dB", true )
   }
   object OutputSpec {
      val aiffFloat  = AudioFileSpec( AudioFileType.AIFF, SampleFormat.Float, 1, 44100.0 ) // numCh, sr not used
      val aiffInt    = AudioFileSpec( AudioFileType.AIFF, SampleFormat.Int24, 1, 44100.0 )
   }
   case class Gain( value: String = "0.0dB", normalized: Boolean = false )

   trait Doc {
      def toProperties( p: Properties ) : Unit
      def className: String
   }

   private object Param {
      val NONE		=	0x0000
      val AMP		=	0x0001
      val TIME		=	0x0002
      val FREQ		=	0x0003
      val PHASE   =	0x0004

      val ABSUNIT		=	0x0000		// ms, Hz, ...
      val ABSPERCENT	=	0x0010		// %
      val RELUNIT		=	0x0020		// +/- ms, +/- Hz, ...
      val RELPERCENT	=	0x0030		// +/- %

      val BEATS		=	0x0100
      val SEMITONES	=	0x0200
      val DECIBEL		=	0x0300

      val FACTOR		   	=	NONE | ABSPERCENT
      val ABS_AMP		   	=	AMP  | ABSUNIT
      val FACTOR_AMP	   	=	AMP  | ABSPERCENT
      val DECIBEL_AMP		=	AMP  | ABSPERCENT	| DECIBEL
      val OFFSET_AMP		   =	AMP  | RELPERCENT
      val ABS_MS			   =	TIME | ABSUNIT
      val ABS_BEATS		   =	TIME | ABSUNIT		| BEATS
      val FACTOR_TIME		=	TIME | ABSPERCENT
      val OFFSET_MS	   	=	TIME | RELUNIT
      val OFFSET_BEATS  	=	TIME | RELUNIT		| BEATS
      val OFFSET_TIME		=	TIME | RELPERCENT
      val ABS_HZ			   =	FREQ | ABSUNIT
      val FACTOR_FREQ		=	FREQ | ABSPERCENT
      val OFFSET_HZ	   	=	FREQ | RELUNIT
      val OFFSET_SEMITONES =	FREQ | RELUNIT		| SEMITONES
      val OFFSET_FREQ	   =	FREQ | RELPERCENT
   }
   private case class Param( value: Double, unit: Int ) {
      override def toString = value.toString + "," + unit.toString
   }

   // ------------- actual processes -------------

   case class UnaryOp( in: String, imagIn: Option[ String ] = None, out: String, imagOut: Option[ String ] = None,
      spec: AudioFileSpec = OutputSpec.aiffFloat, gain: Gain = Gain.immediate,
      offset: String = "0.0", length: String = "1.0", op: String = "thru",
      drive: String = "0.0dB", rectify: Boolean = false, invert: Boolean = false, reverse: Boolean = false,
      dryMix: String = "0.0", dryInvert: Boolean = false, wetMix: String = "1.0" )
   extends Doc {
      def className = "UnaryOpDlg"

      def toProperties( p: Properties ) {
         p.setProperty( "ReInFile", in )
         imagIn.foreach( p.setProperty( "ImInFile", _ ))
         p.setProperty( "HasImInput", imagIn.isDefined.toString )
         p.setProperty( "ReOutFile", out )
         imagOut.foreach( p.setProperty( "ImOutFile", _ ))
         p.setProperty( "HasImOutput", imagOut.isDefined.toString )
         p.setProperty( "OutputType", audioFileType( spec ))
         p.setProperty( "OutputReso", audioFileRes( spec ))
         p.setProperty( "Operator", (op match {
            case "thru"       => 0
            case "sin"        => 1
            case "squared"    => 2
            case "sqrt"       => 3
            case "log"        => 4
            case "exp"        => 5
            case "rectpolar"  => 6
            case "rectpolar_unwrapped"=> 7
            case "polarrect"  => 8
            case "not"        => 9
         }).toString )
         p.setProperty( "GainType", gainType( gain ))
         p.setProperty( "Gain", dbAmp( gain.value ))
         p.setProperty( "Invert", invert.toString )
         p.setProperty( "Reverse", reverse.toString )
         p.setProperty( "DryMix", factorAmp( dryMix ))
         p.setProperty( "DryInvert", dryInvert.toString )
         p.setProperty( "WetMix", factorAmp( wetMix ))
         p.setProperty( "InGain", dbAmp( drive ))

         p.setProperty( "Offset", absMsFactorTime( offset ))
	      p.setProperty( "Length", absMsFactorTime( length ))
      }
   }

   case class BinaryOp( in1: String, imagIn1: Option[ String ] = None,
                        in2: String, imagIn2: Option[ String ] = None, out: String, imagOut: Option[ String ] = None,
      spec: AudioFileSpec = OutputSpec.aiffFloat, gain: Gain = Gain.immediate,
      offset1: String = "0.0", length1: String = "1.0",
      offset2: String = "0.0", length2: String = "1.0",
      op: String = "+",
      drive1: String = "0.0dB", rectify1: Boolean = false, invert1: Boolean = false,
      drive2: String = "0.0dB", rectify2: Boolean = false, invert2: Boolean = false,
      dryMix: String = "0.0", dryInvert: Boolean = false, wetMix: String = "1.0" )
   extends Doc {
      def className = "BinaryOpDlg"

      def toProperties( p: Properties ) {
         p.setProperty( "ReInFile1", in1 )
         p.setProperty( "ReInFile2", in2 )
         imagIn1.foreach( p.setProperty( "ImInFile1", _ ))
         imagIn2.foreach( p.setProperty( "ImInFile2", _ ))
         p.setProperty( "HasImInput1", imagIn1.isDefined.toString )
         p.setProperty( "HasImInput2", imagIn2.isDefined.toString )
         p.setProperty( "ReOutFile", out )
         imagOut.foreach( p.setProperty( "ImOutFile", _ ))
         p.setProperty( "HasImOutput", imagOut.isDefined.toString )
         p.setProperty( "OutputType", audioFileType( spec ))
         p.setProperty( "OutputReso", audioFileRes( spec ))
         p.setProperty( "Operator", (op match {
            case "+"       => 0
            case "*"       => 1
            case "/"       => 2
            case "%"       => 3
            case "pow"     => 4
            case "&"       => 5
            case "|"       => 6
            case "^"       => 7
            case "phase"   => 8
            case "mag"     => 9
            case "min"     => 10
            case "max"     => 11
            case "absmin"  => 12
            case "absmax"  => 13
            case "minsum"  => 14
            case "maxsum"  => 15
            case "minproj" => 16
            case "maxproj" => 17
            case "gate"    => 18
            case "atan"    => 19
         }).toString )
         p.setProperty( "GainType", gainType( gain ))
         p.setProperty( "Gain", dbAmp( gain.value ))
         p.setProperty( "Invert1", invert1.toString )
         p.setProperty( "Invert2", invert2.toString )
         p.setProperty( "Rectify1", rectify1.toString )
         p.setProperty( "Rectify2", rectify2.toString )
         p.setProperty( "DryMix", factorAmp( dryMix ))
         p.setProperty( "DryInvert", dryInvert.toString )
         p.setProperty( "WetMix", factorAmp( wetMix ))
         p.setProperty( "InGain1", dbAmp( drive1 ))
         p.setProperty( "InGain2", dbAmp( drive2 ))

         p.setProperty( "Offset1", absMsFactorTime( offset1 ))
         p.setProperty( "Offset2", absMsFactorTime( offset2 ))
         p.setProperty( "Length1", absMsFactorTime( length1 ))
	      p.setProperty( "Length2", absMsFactorTime( length2 ))
      }
   }

   case class Fourier( in: String, imagIn: Option[ String ] = None, out: String, imagOut: Option[ String ] = None,
      spec: AudioFileSpec = OutputSpec.aiffFloat, gain: Gain = Gain.immediate,
      inverse: Boolean = false, format: String = "cartesian", trunc: Boolean = false,
      memory: Int = 16 )
   extends Doc {
      def className = "FourierDlg"

      def toProperties( p: Properties ) {
         p.setProperty( "ReInFile", in )
         imagIn.foreach( p.setProperty( "ImInFile", _ ))
         p.setProperty( "HasImInput", imagIn.isDefined.toString )
         p.setProperty( "ReOutFile", out )
         imagOut.foreach( p.setProperty( "ImOutFile", _ ))
         p.setProperty( "HasImOutput", imagOut.isDefined.toString )
         p.setProperty( "OutputType", audioFileType( spec ))
         p.setProperty( "OutputReso", audioFileRes( spec ))
         p.setProperty( "Dir", (if( inverse ) 1 else 0).toString )
         p.setProperty( "Format", (format match {
            case "cartesian"  => 0
            case "polar"      => 1
         }).toString )
         p.setProperty( "Length", (if( trunc ) 1 else 0).toString )
         p.setProperty( "Memory", par( memory, Param.NONE ))
         p.setProperty( "GainType", gainType( gain ))
         p.setProperty( "Gain", dbAmp( gain.value ))
      }
   }

   case class Kriechstrom( in: String, out: String, spec: AudioFileSpec = OutputSpec.aiffFloat,
                           gain: Gain = Gain.immediate, length: String = "1.0", minChunks: Int = 4,
                           maxChunks: Int = 4, minRepeats: Int = 1, maxRepeats: Int = 1,
                           minChunkLen: String = "0.02s", maxChunkLen: String = "0.5s",
                           instantaneous: Boolean = true, maxEntry: String = "0.5s",
                           fades: String = "0.2", filterAmount: String = "0.0", filterColor: String = "neutral" )
   extends Doc {
      def className = "KriechstromDlg"

      def toProperties( p: Properties ) {
         p.setProperty( "InputFile", in )
         p.setProperty( "OutputFile", out )
         p.setProperty( "OutputType", audioFileType( spec ))
         p.setProperty( "OutputReso", audioFileRes( spec ))
         p.setProperty( "FltColor", (filterColor match {
            case "dark"    => 0
            case "neutral" => 1
            case "bright"  => 2
         }).toString )
         p.setProperty( "GainType", gainType( gain ))
         p.setProperty( "Gain", dbAmp( gain.value ))
         p.setProperty( "LenUpdate", instantaneous.toString )
         p.setProperty( "MinChunkNum", par( minChunks, Param.NONE ))
	      p.setProperty( "MaxChunkNum", par( maxChunks, Param.NONE ))
         p.setProperty( "MinChunkRep", par( minRepeats, Param.NONE ))
         p.setProperty( "MaxChunkRep", par( maxRepeats, Param.NONE ))
         p.setProperty( "MinChunkLen", absMsTime( minChunkLen ))
	      p.setProperty( "MaxChunkLen", absMsTime( maxChunkLen ))
	      p.setProperty( "CrossFade", absMsFactorTime( fades ))
	      p.setProperty( "EntryPoint", absMsTime( maxEntry ))
         p.setProperty( "FltAmount", factorAmp( filterAmount ))
	      p.setProperty( "OutLength", absMsFactorTime( length ))
//         p.setProperty( "KriechEnv", x )
      }
   }

   // ---- helper ----

   private def absMsFactorTime( s: String ) : String = {
      if( s.endsWith( "s" )) absMsTime( s ) else factorTime( s )
   }

   private def par( value: Double, unit: Int ) : String = Param( value, unit ).toString

   private def dbAmp( s: String ) : String = {
      require( s.endsWith( "dB" ))
      Param( s.substring( 0, s.length - 2 ).toDouble, Param.DECIBEL_AMP ).toString
   }

   private def factorAmp( s: String ) : String = {
      Param( s.toDouble * 100, Param.FACTOR_AMP ).toString
   }

   private def factorTime( s: String ) : String = {
      Param( s.toDouble * 100, Param.FACTOR_TIME ).toString
   }

   private def absMsTime( s: String ) : String = {
      require( s.endsWith( "s" ))
      Param( s.substring( 0, s.length - 1 ).toDouble * 1000, Param.ABS_MS ).toString
   }

   private def gainType( gain: Gain ) : String = {
      (if( gain.normalized ) 0 else 1).toString
   }

   private def audioFileType( spec: AudioFileSpec ) : String = {
      (spec.fileType match {
         case AudioFileType.AIFF => 0x0020
      }).toString
   }

   private def audioFileRes( spec: AudioFileSpec ) : String = {
      (spec.sampleFormat match {
         case SampleFormat.Int16 => 0
         case SampleFormat.Int24 => 1
         case SampleFormat.Float => 2
         case SampleFormat.Int32 => 3            
      }).toString
   }
}