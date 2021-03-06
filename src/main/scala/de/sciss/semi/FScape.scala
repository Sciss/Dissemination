package de.sciss.semi

import java.net.InetSocketAddress
import de.sciss.synth.io.{AudioFileType, SampleFormat, AudioFileSpec}
import java.util.Properties
import de.sciss.osc.{OSCMessage, TCP, OSCClient}
import actors.{TIMEOUT, DaemonActor}
import Dissemination._
import java.io.{IOException, FileOutputStream, File}

// (too?) quickly hacked...
object FScape {
   val verbose       = false
   var OPEN_WINDOW   = false
   val MAX_JOBS      = 1000 

//   @volatile var client: OSCClient = null
//   @volatile var clientReady = false

   private case class Process( name: String, doc: Doc, fun: Boolean => Unit )
   private case class ClientReady( c: OSCClient )
   private case object CreateClient

   private object OSCActor extends DaemonActor {
      start

      def act {
         loop {
            react {
               case CreateClient => {
                  if( verbose ) printInfo( "CreateClient received" )
                  Thread.sleep( 5000 )
                  val c = OSCClient( TCP )
                  c.target = new InetSocketAddress( "127.0.0.1", 0x4653 )
                  var count = 20
                  var ok = false
                  while( count > 0 && !ok ) {
                     count -= 1
                     try {
                        c.start
                        c.action = (msg, addr, when) => JobActor ! msg
//                        client = c
//                        clientReady = true
                        JobActor ! ClientReady( c )
                        ok = true
                        if( verbose ) printInfo( "Connect done" )
                     }
                     catch {
                        case e =>
                           if( verbose ) printInfo( "Connect failed. Sleep" )
                           Thread.sleep( 1000 )
//                        reactWithin( 1000 ) { case TIMEOUT => }
                     }
                  }
               }
            }
         }
      }
   }

   private object JobActor extends DaemonActor {
      var syncID = -1

      start

      def act {
         var client: OSCClient = null
         loop {
            if( verbose ) printInfo( "restartFScape" )
            if( client != null ) {
               client.dispose
               client = null
            }
            val pb = new ProcessBuilder( "/bin/sh", BASE_PATH + fs + "RestartFScape.sh" )
            pb.start()
            OSCActor ! CreateClient
            react {
               case ClientReady( c ) =>
                  client = c
                  if( verbose ) printInfo( "ClientReady received" )
                  var numJobs = 0
                  loopWhile( numJobs < MAX_JOBS ) {
                     react {
                        case Process( name, doc, fun ) /* if( clientReady ) */ => try {
                           numJobs += 1
                           if( verbose ) printInfo( "GOT JOB (" + name + ")" )

                           def timedOut( msg: OSCMessage ) {
                              printInfo( "TIMEOUT (" + name + " -- " + msg + ")" )
                              fun( false )
                              numJobs = math.max( numJobs, MAX_JOBS - 10 ) // this is an indicator of a problem
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

                           val docFile = File.createTempFile( "semi", ".fsc", new File( TEMP_PATH )).getAbsolutePath
                           val prop    = new Properties()
                           prop.setProperty( "Class", "de.sciss.fscape.gui." + doc.className + "Dlg" )
                           doc.toProperties( prop )
                           val os      = new FileOutputStream( docFile )
                           prop.store( os, "Dissemination" )
                           os.close
                           client ! OSCMessage( "/doc", "open", docFile, if( OPEN_WINDOW ) 1 else 0 )
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
               //                                             println( "PROGRESS = " + (p * 100).toInt )
                                                            running  = r
                                                            err      = e
                                                         }
                                                      }
                                                   }
                                                } andThen {
                                                   client ! OSCMessage( addr, "close" )
                                                   if( err != "" ) {
                                                      printInfo( "ERROR (" + name + " -- " + err + ")" + " / " + docFile )
                                                      fun( false )
                                                   } else {
                                                      if( verbose ) printInfo( "Success (" + name + ")" )
                                                      fun( true )
                                                   }
                                                }
                                             }
                                          }
                                       }
                                       case _ => idx += 1
                                    }
                                 } andThen {
                                    if( !found ) {
                                       printInfo( "?! File not found (" + name + " / " + docFile + ")" )
                                       fun( false )
                                    }
                                 }
                              }
                           }
                        } catch {
                           case e: IOException =>
                              printInfo( "Caught exception : " + e )
            //                     printInfo( "ACTIVE ? " + client.isActive + " ; CONNECTED ? " + client.isConnected )
                              fun( false )
                        }
                        case _ =>
                     }
                  }
            }
         }
      }
   }

   private def printInfo( msg: String ) {
      println( "" + new java.util.Date() + " : FScape : " + msg )
   }

   def process( name: String, doc: Doc )( fun: Boolean => Unit ) {
      JobActor ! Process( name, doc, fun )
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
      def className = "BinaryOp"

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

   case class Bleach( in: String, fltIn: Option[ String ] = None, out: String,
      spec: AudioFileSpec = OutputSpec.aiffFloat, gain: Gain = Gain.immediate,
      length: Int = 441, feedback: String = "-60.0dB", clip: String = "18.0dB",
      inverse: Boolean = false )
   extends Doc {
      def className = "Bleach"

      def toProperties( p: Properties ) {
         p.setProperty( "AnaInFile", in )
         fltIn.foreach( p.setProperty( "FltInFile", _ ))
         p.setProperty( "UseAnaAsFilter", fltIn.isEmpty.toString )
         p.setProperty( "OutputFile", out )
         p.setProperty( "OutputType", audioFileType( spec ))
         p.setProperty( "OutputReso", audioFileRes( spec ))
         p.setProperty( "GainType", gainType( gain ))
         p.setProperty( "Gain", dbAmp( gain.value ))
         p.setProperty( "Inverse", inverse.toString )
         p.setProperty( "FilterLength", par( length, Param.NONE ))
         p.setProperty( "FilterClip", dbAmp( clip ))
         p.setProperty( "FeedbackGain", dbAmp( feedback ))
      }
   }

   case class Convolution( in: String, impIn: String, out: String,
      spec: AudioFileSpec = OutputSpec.aiffFloat, gain: Gain = Gain.immediate,
      mode: String = "conv", morphType: String = "rect", length: String = "full",
      truncFade: String = "0.01s", numIRs: Int = 1, winStep: String = "0.02s",
      overlap: String = "0s", normIRs: Boolean = false, trunc: Boolean = false,
      minPhase: Boolean = false )
   extends Doc {
      def className = "Convolution"

      def toProperties( p: Properties ) {
         p.setProperty( "InputFile", in )
         p.setProperty( "ImpulseFile", impIn )
         p.setProperty( "OutputFile", out )
         p.setProperty( "OutputType", audioFileType( spec ))
         p.setProperty( "OutputReso", audioFileRes( spec ))
         p.setProperty( "GainType", gainType( gain ))
         p.setProperty( "Gain", dbAmp( gain.value ))
         p.setProperty( "Mode", (mode match {
            case "conv"    => 0
            case "deconv"  => 1
            case "inv"     => 2
         }).toString )
         p.setProperty( "Policy", (morphType match {
            case "rect"    => 0
            case "polar"   => 1
         }).toString )
         p.setProperty( "Length", (length match {
            case "full"    => 0
            case "input"   => 1
            case "support" => 2
         }).toString )
         p.setProperty( "FadeLen", absMsTime( truncFade ))
         p.setProperty( "IRNumber", par( numIRs, Param.NONE ))
         p.setProperty( "WinStep", absMsTime( winStep ))
         p.setProperty( "WinOverlap", absMsTime( overlap ))
         p.setProperty( "NormImp", normIRs.toString )
         p.setProperty( "TruncOver", trunc.toString )
         p.setProperty( "Morph", (numIRs > 1).toString )
//         p.setProperty( "IRModEnv", x )
         p.setProperty( "MinPhase", minPhase.toString )
      }
   }

   case class Fourier( in: String, imagIn: Option[ String ] = None, out: String, imagOut: Option[ String ] = None,
      spec: AudioFileSpec = OutputSpec.aiffFloat, gain: Gain = Gain.immediate,
      inverse: Boolean = false, format: String = "cartesian", trunc: Boolean = false,
      memory: Int = 16 )
   extends Doc {
      def className = "Fourier"

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

   case class Hilbert( in: String, out: String, imagOut: Option[ String ] = None,
      spec: AudioFileSpec = OutputSpec.aiffFloat, gain: Gain = Gain.immediate,
      freq: Double = 0.0, antiAlias: Boolean = true, envelope: Boolean = false )
   extends Doc {
      def className = "Hilbert"

      def toProperties( p: Properties ) {
         p.setProperty( "InputFile", in )
         p.setProperty( "ReOutFile", out )
         imagOut.foreach( p.setProperty( "ImOutFile", _ ))
         p.setProperty( "OutputType", audioFileType( spec ))
         p.setProperty( "OutputReso", audioFileRes( spec ))
         p.setProperty( "GainType", gainType( gain ))
         p.setProperty( "Gain", dbAmp( gain.value ))
         p.setProperty( "Mode", (
            if( envelope ) 3
            else if( freq == 0.0 ) 0
            else if( freq < 0.0 ) 2
            else 1
         ).toString )
         p.setProperty( "Freq", par( math.abs( freq ), Param.ABS_HZ ))
         p.setProperty( "AntiAlias", antiAlias.toString )
      }
   }

   case class Kriechstrom( in: String, out: String, spec: AudioFileSpec = OutputSpec.aiffFloat,
                           gain: Gain = Gain.immediate, length: String = "1.0", minChunks: Int = 4,
                           maxChunks: Int = 4, minRepeats: Int = 1, maxRepeats: Int = 1,
                           minChunkLen: String = "0.02s", maxChunkLen: String = "0.5s",
                           instantaneous: Boolean = true, maxEntry: String = "0.5s",
                           fades: String = "0.2", filterAmount: String = "0.0", filterColor: String = "neutral" )
   extends Doc {
      def className = "Kriechstrom"

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

   case class Laguerre( in: String, out: String,
      spec: AudioFileSpec = OutputSpec.aiffFloat, gain: Gain = Gain.immediate,
      warp: Double = -0.1, frameSize: Int = 4, overlap: Int = 1 )
   extends Doc {
      def className = "Laguerre"

      def toProperties( p: Properties ) {
         p.setProperty( "InputFile", in )
         p.setProperty( "OutputFile", out )
         p.setProperty( "OutputType", audioFileType( spec ))
         p.setProperty( "OutputReso", audioFileRes( spec ))
         p.setProperty( "GainType", gainType( gain ))
         p.setProperty( "Gain", dbAmp( gain.value ))
         p.setProperty( "Warp", par( warp, Param.FACTOR ))
         p.setProperty( "FrameSize", (frameSize >> 6).toString )
         p.setProperty( "Overlap", (overlap - 1).toString )
      }
   }

   case class MakeLoop( in: String, out: String,
      spec: AudioFileSpec = OutputSpec.aiffFloat, gain: Gain = Gain.immediate,
      length: String = "1s", offset: String = "#auto", trunc: String = "#auto",
      pos: String = "pre", /* shape: String = "normal",*/ cross: String = "eqp" )
   extends Doc {
      def className = "MakeLoop"

      def toProperties( p: Properties ) {
         p.setProperty( "InputFile", in )
         p.setProperty( "OutputFile", out )
         p.setProperty( "OutputType", audioFileType( spec ))
         p.setProperty( "OutputReso", audioFileRes( spec ))
         p.setProperty( "GainType", gainType( gain ))
         p.setProperty( "Gain", dbAmp( gain.value ))
         p.setProperty( "FadePos", (pos match {
            case "pre"  => 0
            case "post" => 1
         }).toString )
//         p.setProperty( "FadeShape", (shape match {
//            case "normal"  => 0
//            case "fast"    => 1
//            case "slow"    => 2
//            case "easy"    => 3
//         }).toString )
         p.setProperty( "FadeType", (cross match {
            case "lin"  => 0
            case "eqp"  => 1
         }).toString )
         p.setProperty( "FadeLen", absMsFactorTime( length ))
         val offset0 = if( offset != "#auto" ) offset else if( pos == "pre" ) length else "0s"
         val trunc0  = if( trunc  != "#auto" ) trunc  else if( pos == "pre" ) "0s" else length 
         p.setProperty( "InitialSkip", absMsFactorTime( offset0 ))
         p.setProperty( "FinalSkip", absMsFactorTime( trunc0 ))
      }
   }

   case class Needlehole( in: String, out: String,
      spec: AudioFileSpec = OutputSpec.aiffFloat, gain: Gain = Gain.immediate,
      filter: String = "median", length: String = "0.05s", thresh: String = "-18dB", subDry: Boolean = false )
   extends Doc {
      def className = "Needlehole"

      def toProperties( p: Properties ) {
         p.setProperty( "InputFile", in )
         p.setProperty( "OutputFile", out )
         p.setProperty( "OutputType", audioFileType( spec ))
         p.setProperty( "OutputReso", audioFileRes( spec ))
         p.setProperty( "GainType", gainType( gain ))
         p.setProperty( "Gain", dbAmp( gain.value ))
         p.setProperty( "Filter", (filter match {
            case "median"  => 0
            case "stddev"  => 1
            case "min"     => 2
            case "center"  => 3
         }).toString )
         p.setProperty( "Length", absMsTime( length ))
         p.setProperty( "Thresh", factorDBAmp( thresh ))
         p.setProperty( "SubDry", subDry.toString )
      }
   }

   case class Resample( in: String, out: String,
      spec: AudioFileSpec = OutputSpec.aiffFloat, gain: Gain = Gain.immediate,
      rate: String = "0semi", keepHeader: Boolean = false, interpolate: Boolean = false,
      fltLength: String = "medium" )
   extends Doc {
      def className = "Resample"

      def toProperties( p: Properties ) {
         p.setProperty( "InputFile", in )
         p.setProperty( "OutputFile", out )
         p.setProperty( "OutputType", audioFileType( spec ))
         p.setProperty( "OutputReso", audioFileRes( spec ))
         p.setProperty( "GainType", gainType( gain ))
         p.setProperty( "Gain", dbAmp( gain.value ))
         p.setProperty( "Quality", (fltLength match {
            case "short"   => 0
            case "medium"  => 1
            case "long"    => 2
         }).toString )
         p.setProperty( "Rate", absRelHzSemiFreq( rate ))
         p.setProperty( "KeepHeader", keepHeader.toString )
         p.setProperty( "Interpole", interpolate.toString )
      }
   }

   case class Rotation( in: String, out: String,
      spec: AudioFileSpec = OutputSpec.aiffFloat, gain: Gain = Gain.immediate,
      mode: String = "rotate", numRepeats: Int = 2, subDry: Boolean = false )
   extends Doc {
      def className = "Rotation"

      def toProperties( p: Properties ) {
         p.setProperty( "InputFile", in )
         p.setProperty( "OutputFile", out )
         p.setProperty( "OutputType", audioFileType( spec ))
         p.setProperty( "OutputReso", audioFileRes( spec ))
         p.setProperty( "GainType", gainType( gain ))
         p.setProperty( "Gain", dbAmp( gain.value ))
         p.setProperty( "Mode", (mode match {
            case "rotate"  => 0
            case "repeat"  => 1
         }).toString )
         p.setProperty( "Repeats", par( numRepeats, Param.NONE ))
         p.setProperty( "SubDry", subDry.toString )
      }
   }

   // XXX SpectPatch

   case class StepBack( in: String, out: String,
      spec: AudioFileSpec = OutputSpec.aiffFloat, gain: Gain = Gain.immediate,
      mode: String = "decon", corrLen: Int = 1024, corrStep: Int = 256, /* corrFine: Int = 32, */
      minSpacing: String = "0.1s", maxSpacing: String = "5.0s", minXFade: String = "0.001s", maxXFade: String ="1.0s",
      offset: String = "0s", weight: Double = 0.5, markers: Boolean = false )
   extends Doc {
      def className = "StepBack"

      def toProperties( p: Properties ) {
         p.setProperty( "InputFile", in )
         p.setProperty( "OutputFile", out )
         p.setProperty( "OutputType", audioFileType( spec ))
         p.setProperty( "OutputReso", audioFileRes( spec ))
         p.setProperty( "GainType", gainType( gain ))
         p.setProperty( "Gain", dbAmp( gain.value ))
         p.setProperty( "Mode", (mode match {
            case "decon"   => 0
            case "random"  => 1
            case "recon"   => 2
            case "forward" => 3
         }).toString )
         p.setProperty( "CorrLength", (math.log( 131072 / corrLen ) / math.log( 2 )).toInt.toString )
         p.setProperty( "CorrStep",  (math.log( 131072 / corrLen ) / math.log( 2 )).toInt.toString )
         p.setProperty( "MinSpacing", absMsTime( minSpacing ))
         p.setProperty( "MaxSpacing", absMsTime( maxSpacing ))
         p.setProperty( "MinXFade", absMsTime( minXFade ))
         p.setProperty( "MaxXFade", absMsTime( maxXFade ))
         p.setProperty( "Offset", offsetMsTime( offset ))
         p.setProperty( "Weight", par( weight * 100, Param.FACTOR_AMP ))
         p.setProperty( "Markers", markers.toString )
      }
   }

   case class Voocooder( in: String, mod: String, out: String,
      spec: AudioFileSpec = OutputSpec.aiffFloat, gain: Gain = Gain.immediate,
      op: String = "*", fltLength: String = "short",
      loFreq: String = "400Hz", hiFreq: String = "11025Hz", /* dryMix: Double = 1.0, wetMix: Double = 0.25, */
      /* rollOff: String = "12semi",*/ bandsPerOct: Int = 12 )
   extends Doc {
      def className = "Voocooder"

      def toProperties( p: Properties ) {
         p.setProperty( "InputFile", in )
         p.setProperty( "ModFile", mod )
         p.setProperty( "OutputFile", out )
         p.setProperty( "OutputType", audioFileType( spec ))
         p.setProperty( "OutputReso", audioFileRes( spec ))
         p.setProperty( "GainType", gainType( gain ))
         p.setProperty( "Gain", dbAmp( gain.value ))
         p.setProperty( "Kombi", (op match {
            case "*"       => 0
            case "%"       => 1
            case "min"     => 2
            case "max"     => 3
            case "vocoder" => 4
         }).toString )
         p.setProperty( "FilterLen", (fltLength match {
            case "short"      => 0
            case "medium"     => 1
            case "long"       => 2
            case "verylong"   => 3
         }).toString )
         p.setProperty( "LoFreq", absHzFreq( loFreq ))
         p.setProperty( "HiFreq", absHzFreq( hiFreq ))
         p.setProperty( "BandsPerOct", par( bandsPerOct, Param.NONE ))
      }
   }

   case class UnaryOp( in: String, imagIn: Option[ String ] = None, out: String, imagOut: Option[ String ] = None,
      spec: AudioFileSpec = OutputSpec.aiffFloat, gain: Gain = Gain.immediate,
      offset: String = "0.0", length: String = "1.0", op: String = "thru",
      drive: String = "0.0dB", rectify: Boolean = false, invert: Boolean = false, reverse: Boolean = false,
      dryMix: String = "0.0", dryInvert: Boolean = false, wetMix: String = "1.0" )
   extends Doc {
      def className = "UnaryOp"

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

   case class Wavelet( in: String, out: String,
      spec: AudioFileSpec = OutputSpec.aiffFloat, gain: Gain = Gain.immediate,
      filter: String = "daub4", inverse: Boolean = false, trunc: Boolean = false,
      scaleGain: String = "3dB" )
   extends Doc {
      def className = "Wavelet"

      def toProperties( p: Properties ) {
         p.setProperty( "InputFile", in )
         p.setProperty( "OutputFile", out )
         p.setProperty( "OutputType", audioFileType( spec ))
         p.setProperty( "OutputReso", audioFileRes( spec ))
         p.setProperty( "Dir", (if( inverse ) 1 else 0).toString )
         p.setProperty( "Filter", (filter match {
            case "daub4"   => 0
            case "daub6"   => 1
            case "daub8"   => 2
            case "daub10"  => 3
            case "daub12"  => 4
            case "daub14"  => 5
            case "daub16"  => 6
            case "daub18"  => 7
            case "daub20"  => 8
         }).toString )
         p.setProperty( "Length", (if( trunc ) 1 else 0).toString )
         p.setProperty( "ScaleGain", dbAmp( scaleGain ))
         p.setProperty( "GainType", gainType( gain ))
         p.setProperty( "Gain", dbAmp( gain.value ))
      }
   }

   // ---- helper ----

   private def absMsFactorTime( s: String ) : String = {
      if( s.endsWith( "s" )) absMsTime( s ) else factorTime( s )
   }

   private def par( value: Double, unit: Int ) : String = Param( value, unit ).toString

   private def absRelHzSemiFreq( s: String ) : String = {
      if( s.endsWith( "semi" )) semiFreq( s )
      else if( s.endsWith( "Hz" )) {
         if( s.startsWith( "+" ) || s.startsWith( "-" )) relHzFreq( s ) else absHzFreq( s )
      } else offsetFreq( s )
   }

   private def semiFreq( s: String ) : String = {
      require( s.endsWith( "semi" ))
      Param( s.substring( 0, s.length - 4 ).toDouble, Param.OFFSET_SEMITONES ).toString
   }

   private def relHzFreq( s: String ) : String = {
      require( s.endsWith( "Hz" ))
      Param( s.substring( 0, s.length - 2 ).toDouble, Param.OFFSET_HZ ).toString
   }

   private def absHzFreq( s: String ) : String = {
      require( s.endsWith( "Hz" ))
      Param( s.substring( 0, s.length - 2 ).toDouble, Param.ABS_HZ ).toString
   }

   private def offsetFreq( s: String ) : String = {
      Param( s.toDouble * 100, Param.OFFSET_FREQ ).toString
   }

   private def dbAmp( s: String ) : String = {
      require( s.endsWith( "dB" ))
      Param( s.substring( 0, s.length - 2 ).toDouble, Param.DECIBEL_AMP ).toString
   }

   private def factorDBAmp( s: String ) : String = {
      if( s.endsWith( "dB" )) dbAmp( s ) else factorAmp( s )
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

   private def offsetMsTime( s: String ) : String = {
      require( s.endsWith( "s" ))
      Param( s.substring( 0, s.length - 1 ).toDouble * 1000, Param.OFFSET_MS ).toString
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