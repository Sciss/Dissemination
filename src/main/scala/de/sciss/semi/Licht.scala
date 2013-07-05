package de.sciss.semi

import de.sciss.synth
import synth.proc.{DSL, Proc, ProcTxn, Ref}
import synth.ugen._
import synth.GE
import DSL._
import Util._

object Licht {
   val verbose = false

   val MIN_WIDTH     = 5
   val MAX_WIDTH     = 50
   val MIN_FADE      = 6.6
   val MAX_FADE      = 66.6
   val MIN_DUST_DUR  = 73.0
   val MAX_DUST_DUR  = 103.0
}

class Licht(proc: Proc) extends SemiProcess {
  import Licht._

  private val activeRef = Ref(false)

  def active( implicit tx: ProcTxn ) = activeRef()
  def active_=(onOff: Boolean)(implicit tx: ProcTxn) {
    val wasActive = activeRef.swap(onOff)
    if (wasActive == onOff) return
    if (onOff) start() else stop()
  }

  def name        = "licht"
  def exclusives  = Set.empty[String]

  def trigger: GE = {
    // XXX
    import synth._
    //      Dust.kr( 1.0 / DUST_DUR )
    Dust.kr(LFNoise0.kr(1.0 / 107).linexp(-1, 1, 1.0 / MAX_DUST_DUR, 1.0 / MIN_DUST_DUR))
  }

  private def stop()(implicit tx: ProcTxn) {}

  private def start()(implicit tx: ProcTxn) {
    if (verbose) println(s"${new java.util.Date()} : Licht AN")
    glide(rrand(MIN_FADE, MAX_FADE)) {
      val pcon  = proc.control("pos")
      val width = rrand(MIN_WIDTH, MAX_WIDTH)
      pcon                 .v_=(if (pcon.v == 0.0) width + 18 else 0)
      proc.control("width").v_=(width)
    }
    ProcHelper.whenGlideDone(proc, "pos") { implicit tx =>
      if (verbose) println(s"${new java.util.Date()} : Licht AUS")
      active_=(onOff = false)
    }
  }
}