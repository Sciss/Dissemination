package de.sciss.semi

import Dissemination._
import SemiNuages._
import Util._
import de.sciss.synth
import synth.proc.{Proc, ExpWarp, ProcDemiurg, DSL, ParamSpec, ProcTxn, Ref}
import collection.immutable.{ IndexedSeq => IIdxSeq }
import DSL._
import annotation.tailrec

object Sprenger {
   val FADE_DUR   = 6.0
   val MIN_KEEP   = 45.0
   val MIN_SKIP   = 27.0
   val MAX_SKIP   = 67.0
   val INOUT_MIN  = 16.0
   val INOUT_MAX  = 48.0
   val SHORT_FADE = 3.0
}

class Sprenger {
   import Sprenger._

   private val urn = new Urn( (0 until NUM_PLATES): _* )

   private val ch1: Ref[ Option[ Channel ]] = Ref( None )
   private val ch2: Ref[ Option[ Channel ]] = Ref( None )
   private val activeRef = Ref( false )
   private val cutsRef = Ref( Seq.empty[ (Double, Double) ])

   def active( implicit tx: ProcTxn ) = activeRef()
   def active_=( onOff: Boolean )( implicit tx: ProcTxn ) {
      val wasActive = activeRef.swap( onOff )
      if( wasActive == onOff ) return
      if( onOff ) start else stop
   }

   @tailrec
   private def cutOut( pieces: IIdxSeq[ (Double, Double) ], minKeep: Double, minSkip: Double, maxSkip: Double ) : IIdxSeq[ (Double, Double) ] = {
      val minPcsLen  = minKeep + minKeep + minSkip
      val f = pieces.filter( tup => tup._2 - tup._1 >= minPcsLen )
      if( f.size == 0 ) return pieces
      val piece = f( rand( f.size ))
      val idx = pieces.indexOf( piece )
      val pieceDur = piece._2 - piece._1
      val cutDur = exprand( minSkip, math.min( pieceDur - (minKeep + minKeep), maxSkip ))
      val cutPos = exprand( minKeep, pieceDur - (minKeep + cutDur) )
      val pc1 = piece._1 -> (piece._1 + cutPos)
      val pc2 = (piece._1 + cutPos + cutDur) -> piece._2
      cutOut( pieces.patch( idx, pc1 :: pc2 :: Nil, 1 ), MIN_KEEP, MIN_SKIP, MAX_SKIP )
   }

   private def start( implicit tx: ProcTxn ) {
      val Seq( idx1, idx2 ) = urn.take( 2 )
//      ch1.set( ch1v )
//      ch2.set( ch2v )

      val cuts0 = cutOut( Vector( 0.0 -> 567.74122 ), 45, 27, 67 )
      cutsRef.set( cuts0.tail )
      val cut = cuts0.head
      val chan1 = createOne( idx1, "L", cut )
      val chan2 = createOne( idx2, "R", cut )
      ch1.set( Some( chan1 ))
      ch2.set( Some( chan2 ))

      xfade( SHORT_FADE ) {
         chan1.procFilter.engage
         chan2.procFilter.engage
      }
      glide( exprand( INOUT_MIN, INOUT_MAX )) {
         chan1.procFilter.control( "fade" ).v = 1
      }
      glide( exprand( INOUT_MIN, INOUT_MAX )) {
         chan2.procFilter.control( "fade" ).v = 1
      }
   }

   private def cutDone( ext: String ) {
      if( ext == "L" ) {
         ProcTxn.spawnAtomic( implicit tx => if( active ) {
            val cuts0 = cutsRef()
            if( cuts0.nonEmpty ) {
               xfade( FADE_DUR ) {
                  val cut  = cuts0.head
                  val cuts = cuts0.tail
                  cutsRef.set( cuts )
                  ch1().foreach( ch => setCut( ch.procGen, cut ))
                  ch2().foreach( ch => setCut( ch.procGen, cut ))
               }
            } else {
               active = false
            }
         })
      }
   }

   private def createOne( idx: Int, ext: String, cut: (Double, Double) )( implicit tx: ProcTxn ) : Channel = {
      import synth._
      import ugen._

      val procFilter = factory( "sprenger-trans" ).make
      val procGen    = (gen( "sprenger-" + ext ) {
         val pamp = pControl( "amp", ParamSpec( 0.dbamp, 18.dbamp, ExpWarp ), 12.dbamp )
         val ppos = pScalar( "pos", ParamSpec( 0, 600), 1 )
         val pdur = pScalar( "dur", ParamSpec( 1, 600), 1 )
         graph {
            val path       = AUDIO_PATH + fs + "080304_173812_MisionSprengerOKM-" + ext + ".aif"
            val startFrame = (ppos.v * 44100L).toLong // AudioFileCache.spec( path ).numFrames
            val b = bufCue( path, startFrame )
            val done = Done.kr( Line.kr( dur = pdur.ir - FADE_DUR ))
            done.react { cutDone( ext )}
            DiskIn.ar( 1, b.id ) * pamp.kr
         }
      }).make

      val plate = plates( idx )
      val insertTarget = ProcHelper.findOutEdge( plate.collector, collMaster ).get.in
      plate.collector ~|procFilter|> insertTarget
      procGen ~> procFilter.audioInput( "in2" )
      procFilter.bypass
      setCut( procGen, cut )
      procGen.play
      procFilter.play

      Channel( idx, procFilter, procGen )
   }

   private def setCut( p: Proc, cut: (Double, Double) )( implicit tx: ProcTxn ) {
      p.control( "pos" ).v = cut._1
      p.control( "dur" ).v = cut._2 - cut._1
   }

   private def stop( implicit tx: ProcTxn ) {
//      ProcHelper.stopAndDispose( )
      val fdt = exprand( INOUT_MIN, INOUT_MAX )
      val chan1O = ch1.swap( None )
      val chan2O = ch2.swap( None )
      glide( fdt ) {
         (chan1O :: chan2O :: Nil) foreach( _.foreach { ch =>
            ch.procFilter.control( "fade" ).v = 0
            ProcHelper.whenGlideDone( ch.procFilter, "fade" ) { implicit tx =>
               ProcHelper.stopAndDispose( SHORT_FADE, ch.procFilter, postFun = ch.procGen.dispose( _ ))
            }
         })
      }
   }

   private case class Channel( idx: Int, procFilter: Proc, procGen: Proc )
}