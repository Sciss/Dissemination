/*
 *  Windspiel.scala
 *  (Dissemination)
 *
 *  Copyright (c) 2010-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.semi

import de.sciss.synth
import de.sciss.synth.ugen
import ugen._
import synth._
import proc.{ProcDemiurg, DSL, Proc, ParamSpec, ExpWarp, AudioFileCache, ProcTxn}
import Util._
import Dissemination._
import collection.immutable.{ IndexedSeq => IIdxSeq }
import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import javax.swing.SwingWorker
import java.io.{File, RandomAccessFile}
import DSL._
import SemiNuages._

object Windspiel {
  val verbose       = false

  val MIN_LOSKEW    = 0.25
  val MAX_LOSKEW    = 1.0
  val MIN_HISKEW    = 1.0
  val MAX_HISKEW    = 4.0
  val MIN_RATE      = 1.0
  val MAX_RATE      = 2.0
  val MIN_LOSTEP    = 0.05
  val MAX_LOSTEP    = 0.2
  val MIN_HISTEP    = 0.4
  val MAX_HISTEP    = 1.0
  val MIN_POW       = 1.2
  val MAX_POW       = 2.0
  val MIN_DUR       = 20.0
  val MAX_DUR       = 60.0
  val REC_PROB      = 0.2

  val MIN_SPEED     = 0.25 // 4
  val MAX_SPEED     = 0.6
  val AMP           = 9.0 // 4.5

  lazy val SF_PATH    = AUDIO_PATH + fs + "Windspiel2.aif"
  lazy val SPAN_FILE  = AUDIO_PATH + fs + "windspiel_spans"

  lazy val df = SynthDef("wind") {
    val bufID = "buf" .ir
    val rate  = "rate".kr(1)
    val dur   = "dur" .ir
    val play  = VDiskIn.ar(2, bufID, rate)
    val env   = Line.ar(1, 0, dur, freeSelf)
    Out.ar(0, play * env)
  }

  case class Span(start: Long, stop: Long) {
    def length = stop - start
  }

  lazy val spans = {
    val raf = new RandomAccessFile(SPAN_FILE, "r")
    val numTones = raf.readInt()
    try {
      Vector.fill(numTones) {
        val numVariants = raf.readInt()
        Vector.fill(numVariants) {
          val start = raf.readLong()
          val stop  = raf.readLong()
          Span(start, stop)
        }
      }
    } finally {
      raf.close()
    }
  }
  lazy val spansFlat    = spans.flatten
  lazy val numSpansFlat = spansFlat.size
}

class Windspiel extends BasicProcess {

  import Windspiel._

  private val urn = new Urn(0 until NUM_PLATES: _*)

  def name        = "windspiel"
  def exclusives  = Set.empty[String]

  def trigger: GE = {
    // XXX
    import synth._
    Dust.kr(LFNoise0.kr(1.0 / 61).linexp(-1, 1, 1.0 / 10, 1.0 / 60))
  }

  protected def stop()(implicit tx: ProcTxn) {}

  protected def start()(implicit tx: ProcTxn) {
    val so                = Server.Config()
    val recPathF          = ProcHelper.createTempAudioFile()
    so.nrtOutputPath      = recPathF.getAbsolutePath // "/Users/hhrutz/Desktop/test.aif"
    so.sampleRate         = 44100
    so.outputBusChannels  = 2
    so.programPath        = CMD_SCSYNTH

    val rate              = exprand(MIN_RATE, MAX_RATE)

    val bc = BounceSynthContext(so)
    bc.perform(bc.add(df.recvMsg))

    var bufID     = 0
    var nodeID    = 1000
    var stopTime  = 0.0
    val g         = new Group(s, 0)

    val loSkew    = exprand(MIN_LOSKEW, MAX_LOSKEW)
    val hiSkew    = exprand(MIN_HISKEW, MAX_HISKEW)

    val totalDur  = exprand(MIN_DUR, MAX_DUR)
    val minStep   = exprand(MIN_LOSTEP, MAX_LOSTEP)
    val maxStep   = exprand(MIN_HISTEP, MAX_HISTEP)
    val m         = exprand(MIN_POW, MAX_POW)
    val steps     = calcSteps(totalDur, minStep, maxStep, m)
    val n         = steps.size
    val numBufs   = n // 30 // XXX should calculate properly

    steps.zipWithIndex foreach { case (dt,step) =>
      bc.perform {
        val idx         = rrand(0.0, 1.0).pow(step.linexp(0, n - 1, loSkew, hiSkew))
          .linlin(0, 1, 0, numSpansFlat).toInt
        val span        = spansFlat(idx)
        val startFrame  = span.start.toInt
        val dur         = span.length * rate / 44100.0
        val b           = new Buffer(s, bufID)
        bufID           = (bufID + 1) % numBufs
        bc.add(b.closeMsg(b.freeMsg(release = false)))
        bc.add(b.allocMsg(32768, 2, b.cueMsg(SF_PATH, startFrame)))
        val node        = new Synth(s, nodeID)
        nodeID         += 1
        bc.add(node.newMsg("wind", g, Seq("buf" -> bufID, "rate" -> rate, "dur" -> dur)))
        stopTime        = stopTime.max(bc.timebase + dur)
      }
      bc.timebase += dt
    }

    val genLog = f"gen-wind rate $rate%1.3f lo-skew $loSkew%1.3f hi-skew $hiSkew%1.3f min-step $minStep%1.3f max-step $maxStep%1.3f pow $m%1.3f steps $n dur ${(stopTime * 44100L).toLong}"

    bc.timebase = (stopTime + 0.1).max(bc.timebase)
    bc.perform(bc.add(osc.NodeRunMessage(0 -> true))) // dummy
    val worker = bc.render
    worker.addPropertyChangeListener(new PropertyChangeListener {
      def propertyChange(e: PropertyChangeEvent) {
        //            println( "EVENT = " + e.getPropertyName() + "; " + e.getOldValue() + " -> " + e.getNewValue() )
        if (e.getPropertyName == "state" && e.getNewValue == SwingWorker.StateValue.DONE) {
          ProcTxn.atomic { implicit tx =>
            processFile(genLog, recPathF)
          }
        }
      }
    })
    worker.execute()
  }

  private def processFile(genLog: String, recPathF: File)(implicit tx: ProcTxn) {
    if (!active) return

    val tmpAF     = ProcHelper.createTempAudioFile()
    val outPathF  = ProcHelper.createTempAudioFile()
    val tmpA      = tmpAF   .getAbsolutePath
    val outPath   = outPathF.getAbsolutePath
    val recPath   = recPathF.getAbsolutePath

    // val numFrames0  = AudioFileCache.spec(recPath).numFrames
    // val numFrames   = nextPowerOfTwo(numFrames0)

    import FScape._

    val daub = if (coin(0.8)) "daub4" else "daub16"

    val docWT = Wavelet(
      in      = recPath,
      out     = tmpA,
      filter  = daub
    )
    val scale = rrand(1, 3)
    //      val stop    = numFrames >> (scale - 1)
    //      val start   = numFrames >> scale
    val offLen = 2.0.pow(-scale)
    //      val dur     = offLen * numFrames / 44100
    val docCut  = UnaryOp(
      in      = tmpA,
      out     = outPath,
      offset  = offLen.toString,
      length  = offLen.toString,
      spec    = OutputSpec.aiffInt,
      gain    = Gain.normalized
    )

    val procLog = s"filter-wind $daub scale $scale"

    processChain("WindspielWT", docWT :: docCut :: Nil) { success =>
      ProcTxn.spawnAtomic { implicit tx => // XXX spawn?
        if (success) {
          if (verbose) println(this.toString + " RENDER DONE")
          //               neighbour.inject( outPath )
          createProc(genLog, procLog, outPathF /* , dur */)
          tmpAF   .delete()
          recPathF.delete()
          //println( "REC WAS : " + recPathF.getAbsolutePath() )
          //println( "WT  WAS : " + tmpAF.getAbsolutePath() )
        }
        //            recording.set( false )
      }
    }
  }

  private def createProc(genLog: String, procLog: String, outPathF: File /*, dur: Double*/)(implicit tx: ProcTxn) {
    if (!active) return
    active_=(onOff = false)

    lazy val p: Proc = (ProcDemiurg.factories.find( _.name == name ) getOrElse gen( name ) {
      val pspeed  = pControl ("speed", ParamSpec(0.1f , 10, ExpWarp), 1)
      val pamp    = pAudio   ("amp"  , ParamSpec(0.001, 10, ExpWarp), 1)
      val pdur    = pScalar  ("dur"  , ParamSpec(0, 600), 1)
      val pout2   = pAudioOut("out2")
      graph {
        val buf     = bufCue(outPathF.getAbsolutePath)
        val bufID   = buf.id
        val speed   = pspeed.kr // * BufRateScale.ir( bufID )
        val d       = VDiskIn.ar(2, bufID, speed)
        val done    = Done.kr(Line.kr(dur = pdur.ir))
        done.react {
          if (verbose) println("" + new java.util.Date() + " DONE PLAYING " + p)
          ProcTxn.spawnAtomic { implicit tx =>
            ProcHelper.stopAndDispose(0, p)
          }
        }
        // val Seq( sigL, sigR ) = (d * pamp.ar).outputs
        val sigLR = d * pamp.ar
        val sigL = sigLR \ 0
        val sigR = sigLR \ 1
        pout2.ar(sigR)
        sigL
      }
    }).make

    val speed = exprand(MIN_SPEED, MAX_SPEED)
    p.control("speed").v_=(speed)
    p.control("amp"  ).v_=(AMP.dbamp)
    //      p.control( "dur" ).v    = dur / speed + 0.2
    val afs = AudioFileCache.spec(outPathF.getAbsolutePath)
    // println( outPathF.getAbsolutePath() )
    val dur = afs.numFrames / (afs.sampleRate * speed) + 0.2
    p.control("dur").v_=(dur)
    if (verbose) println("" + new java.util.Date() + " STARTING (SPARSE) " + p)

    val Seq(idx1, idx2) = urn.take(2)
    val collCoin1   = coin(REC_PROB)
    val collCoin2   = coin(REC_PROB)
    val collector1  = if (collCoin1) plates(idx1).collector1 else plates(idx1).collector2
    val collector2  = if (collCoin2) plates(idx2).collector1 else plates(idx2).collector2

    p ~> collector1
    p.audioOutput("out2") ~> collector2
    p.play

    Analysis.log(genLog )
    Analysis.log(procLog)
    Analysis.log(f"$name-play speed $speed%1.3f idx1 $idx1 idx2 $idx2 coin1 $collCoin1 coin2 $collCoin2 dur ${(dur * 44100L).toLong}")
  }

  def calcStep(minStep: Double, maxStep: Double, m: Double, num: Int)(n: Int) = {
    val k = num * 0.5
    ((n - k) / k).abs.pow(m) * (maxStep - minStep) + minStep
  }

  def calcSteps(dur: Double, minStep: Double, maxStep: Double, m: Double) = {
    var num   = 1
    var step  = 128
    var steps = IIdxSeq.empty[Double]
    do {
      val f = calcStep(minStep, maxStep, m, num) _
      steps = IIdxSeq.tabulate(num)(f)
      val stepsDur = steps.sum
      if (stepsDur < dur) {
        if (step < 0) step = -step / 2
      } else {
        if (step > 0) step = -step / 2
      }
      num += step
    } while (step.abs > 0 && num > 0)
    // num.max( 1 )
    steps
  }
}