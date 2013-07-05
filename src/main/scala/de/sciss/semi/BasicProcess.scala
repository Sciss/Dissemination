package de.sciss.semi

import de.sciss.synth.proc.{ProcTxn, Ref}

trait BasicProcess extends SemiProcess {
  protected def start()(implicit tx: ProcTxn): Unit
  protected def stop ()(implicit tx: ProcTxn): Unit

  private val activeRef = Ref(initialValue = false)

  final def active(implicit tx: ProcTxn) = activeRef()
  final def active_=(onOff: Boolean)(implicit tx: ProcTxn) {
    val wasActive = activeRef.swap(onOff)
    if (wasActive == onOff) return
    if (onOff) {
      Analysis.log(s"start-proc $name")
      start()
    } else {
      Analysis.log(s"stop-proc $name")
      stop()
    }
  }

}