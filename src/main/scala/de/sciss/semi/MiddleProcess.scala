package de.sciss.semi

import de.sciss.synth.proc.{Ref, ProcTxn}
import de.sciss.semi.Util._

trait MiddleProcess extends BasicProcess {
  protected def minFade   : Double
  protected def maxFade   : Double
  protected def engageFade: Double

  final protected def fadeTime(): Double = exprand(minFade, maxFade)
}