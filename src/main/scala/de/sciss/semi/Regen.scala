package de.sciss.semi

import Dissemination._
import de.sciss.synth
import synth.proc.{ExpWarp, ParamSpec, DSL, ProcTxn, Proc}
import DSL._
import Util._

object Regen {
   val INOUT_MIN  = 32.0
   val INOUT_MAX  = 64.0
   val SHORT_FADE = 3.0
}

class Regen extends WaterLike {
   import Regen._

   protected def minFade      = INOUT_MIN
   protected def maxFade      = INOUT_MAX
   protected def engageFade   = SHORT_FADE

   protected def filters( implicit tx: ProcTxn ) : (Proc, Proc) = {
      val fact = factory( "water-trans" + rrand( 1, 2 ))
      (fact.make, fact.make)
   }

   protected def gens( implicit tx: ProcTxn ) : (Proc, Proc) = {
      import synth._
      import ugen._
      
      val Seq( gen1, gen2 ) = ("L" :: "R" :: Nil) map { ext =>
         val g = (gen( "sprenger-" + ext ) {
            val pamp = pControl( "amp", ParamSpec( 0.dbamp, 18.dbamp, ExpWarp ), 3.dbamp )
            val ppos = pScalar( "pos", ParamSpec( 0, 900), 1 )
            graph {
               val path       = AUDIO_PATH + fs + "080227_WeimarRegenFensterAT-" + ext + ".aif"
               val startFrame = (ppos.v * 44100L).toLong // AudioFileCache.spec( path ).numFrames
               val b          = bufCue( path, startFrame )
               val disk       = DiskIn.ar( 1, b.id ) * pamp.kr
               val done       = Done.kr( disk )
               done.react { diskDone( ext )}
               disk
            }
         }).make
         g.control( "pos" ).v = rrand( 0.0, 300.0 )
         g
      }
      (gen1, gen2)
   }

   private def diskDone( ext: String ) {
      if( ext == "L" ) {
         ProcTxn.spawnAtomic( implicit tx => active = false )
      }
   }
}