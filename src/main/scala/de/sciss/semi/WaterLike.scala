package de.sciss.semi

import Dissemination._
import SemiNuages._
import de.sciss.synth.proc.{DSL, ProcFactory, ProcTxn, Ref, Proc}
import DSL._
import Util._

object WaterLike {
   case class Channel( idx: Int, procFilter: Proc, procGen: Proc )
//   protected abstract sealed class Side( val extension: String )
//   case object Left extends Side( "L" )
//   case object Right extends Side( "R" )
}

trait WaterLike extends SemiProcess {
   import WaterLike._
   
   private val urn = new Urn( (0 until NUM_PLATES): _* )

   protected val ch1: Ref[ Option[ Channel ]] = Ref( None )
   protected val ch2: Ref[ Option[ Channel ]] = Ref( None )
   private val activeRef = Ref( false )

   // --- abstract ---
   protected def filters( implicit tx: ProcTxn ) : (Proc, Proc)
   protected def gens( implicit tx: ProcTxn ) : (Proc, Proc)
   protected def minFade : Double
   protected def maxFade : Double
   protected def engageFade : Double
   
   def active( implicit tx: ProcTxn ) = activeRef()
   def active_=( onOff: Boolean )( implicit tx: ProcTxn ) {
      val wasActive = activeRef.swap( onOff )
      if( wasActive == onOff ) return
      if( onOff ) start else stop
   }

   private def start( implicit tx: ProcTxn ) {
      val Seq( idx1, idx2 ) = urn.take( 2 )
//      ch1.set( ch1v )
//      ch2.set( ch2v )

//      val cuts0 = cutOut( Vector( 0.0 -> 567.74122 ), 45, 27, 67 )
//      cutsRef.set( cuts0.tail )
//      val cut = cuts0.head
//      val chan1 = createOne( idx1, "L", cut )
//      val chan2 = createOne( idx2, "R", cut )
//      ch1.set( Some( chan1 ))
//      ch2.set( Some( chan2 ))

      val (flt1, flt2)  = filters
      val (gen1, gen2)  = gens
      val chan1         = Channel( idx1, flt1, gen1 )
      ch1.set( Some( chan1 ))
      val chan2         = Channel( idx2, flt2, gen2 )
      ch2.set( Some( chan2 ))
      val chans         = chan1 :: chan2 :: Nil

      chans foreach { ch =>
         val plate = plates( ch.idx )
         val insertTarget = ProcHelper.findOutEdge( plate.collector, collMaster ).get.in
         plate.collector ~| ch.procFilter |> insertTarget
         ch.procGen ~> ch.procFilter.audioInput( "in2" )
         ch.procFilter.bypass
         ch.procGen.play
         ch.procFilter.play
      }

      chans foreach { ch =>
         xfade( engageFade ) {
            ch.procFilter.engage
         }
         glide( exprand( minFade, maxFade )) {
            ch.procFilter.control( "fade" ).v = 1
         }
      }
   }

   private def stop( implicit tx: ProcTxn ) {
//      ProcHelper.stopAndDispose( )
      val fdt = exprand( minFade, maxFade )
      val chan1O = ch1.swap( None )
      val chan2O = ch2.swap( None )
      glide( fdt ) {
         (chan1O :: chan2O :: Nil) foreach( _.foreach { ch =>
            ch.procFilter.control( "fade" ).v = 0
            ProcHelper.whenGlideDone( ch.procFilter, "fade" ) { implicit tx =>
               ProcHelper.stopAndDispose( engageFade, ch.procFilter, postFun = ch.procGen.dispose( _ ))
            }
         })
      }
   }
}