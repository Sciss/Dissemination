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

class Sprenger extends WaterLike {
   import Sprenger._
   import WaterLike._

   private val cutsRef = Ref( Seq.empty[ (Double, Double) ])

   protected def minFade      = INOUT_MIN
   protected def maxFade      = INOUT_MAX
   protected def engageFade   = SHORT_FADE

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

   protected def filters( implicit tx: ProcTxn ) : (Proc, Proc) = {
      val fact = factory( "water-trans" + rrand( 1, 2 ))
      (fact.make, fact.make)
   }

   protected def gens( implicit tx: ProcTxn ) : (Proc, Proc) = {
      import synth._
      import ugen._

      val cuts0 = cutOut( Vector( 0.0 -> 567.74122 ), 45, 27, 67 )
      cutsRef.set( cuts0.tail )
      val cut = cuts0.head

      val Seq( gen1, gen2 )= ("L" :: "R" :: Nil) map { ext =>
         val g = (gen( "sprenger-" + ext ) {
            val pamp = pControl( "amp", ParamSpec( 0.dbamp, 18.dbamp, ExpWarp ), 12.dbamp )
            val ppos = pScalar( "pos", ParamSpec( 0, 600), 1 )
            val pdur = pScalar( "dur", ParamSpec( 1, 600), 1 )
            graph {
               val path       = AUDIO_PATH + fs + "080304_173812_MisionSprengerOKM-" + ext + ".aif"
               val startFrame = (ppos.v * 44100L).toLong // AudioFileCache.spec( path ).numFrames
               val b          = bufCue( path, startFrame )
               val done       = Done.kr( Line.kr( dur = pdur.ir - FADE_DUR ))
               done.react { cutDone( ext )}
               DiskIn.ar( 1, b.id ) * pamp.kr
            }
         }).make
         setCut( g, cut )
         g
      }
      (gen1, gen2)
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
   
   private def setCut( p: Proc, cut: (Double, Double) )( implicit tx: ProcTxn ) {
      p.control( "pos" ).v = cut._1
      p.control( "dur" ).v = cut._2 - cut._1
   }
}