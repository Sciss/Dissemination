package de.sciss.semi

import de.sciss.synth.proc.ProcTxn

trait SemiProcess {
   def active( implicit tx: ProcTxn ) : Boolean
   def active_=( onOff: Boolean )( implicit tx: ProcTxn ) : Unit
}