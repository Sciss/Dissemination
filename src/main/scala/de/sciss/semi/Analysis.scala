package de.sciss.semi

import scala.annotation.elidable
import de.sciss.synth.proc.{TxnLocal, ProcTxn}

object Analysis {
  var enabled = true

  private var timeZero  = 0L
  private val sync      = new AnyRef

  private val txnTime   = TxnLocal(System.currentTimeMillis())

  @elidable(elidable.INFO) def log(what: => String)(implicit tx: ProcTxn) {
    if (enabled) {
      val now   = txnTime()(tx)
      val time  = sync.synchronized {
        if (timeZero == 0L) timeZero = now
        now - timeZero
      }
      val timeF = (time * 44.1).toLong
      tx.afterCommit(_ => println(s"<ANA> $timeF $what"))
    }
  }
}