/*
 *  Zeven.scala
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

import de.sciss.synth._
import ugen._
import SemiNuages._
import Dissemination._
import de.sciss.synth.proc.{DSL, ProcTxn, Proc, ExpWarp, ParamSpec}
import DSL._
import Util._

object Zeven {
   val MIN_FILTER_FADE  =  18.0 // s
   val MAX_FILTER_FADE  =  30.0 // s
   val MIN_DUR          =  45.0 // s
   val MAX_DUR          =  60.0 // s
   val FILTER_GAIN      = -21.0 // dB
   val TAPE_GAIN        =   6.0 // dB
   val MIN_TRIG_FREQ    = 1.0 / 300
   val MAX_TRIG_FREQ    = 1.0 / 180
   val TRIG_CHANGE_FREQ = 1.0 / 607
}

class Zeven(val idx: Int) extends ColorLike {
  import Zeven._

  protected def minFade     = MIN_FILTER_FADE
  protected def maxFade     = MAX_FILTER_FADE
  protected def engageFade  = 0.1
  protected def delayGen    = false

  def name = "zeven"

  def exclusives = Set.empty[String]

  def trigger: GE = {
    Dust.kr(LFNoise0.kr(TRIG_CHANGE_FREQ).linexp(-1, 1, MIN_TRIG_FREQ, MAX_TRIG_FREQ))
  }

  def gen1(implicit tx: ProcTxn): Proc = {
      val g = gen(name) {
        val pamp = pControl("amp", ParamSpec(0.dbamp, 18.dbamp, ExpWarp), TAPE_GAIN.dbamp)
        val ppos = pScalar ("pos", ParamSpec(0, 1800), 1)
        val pdur = pScalar ("dur", ParamSpec(0.2, 600), 1)
        graph {
          val path = AUDIO_PATH + fs + "MiscZeven01Ed2-M.aif"
          val startFrame = (ppos.v * 44100L).toLong // AudioFileCache.spec( path ).numFrames
          val b = bufCue(path, startFrame)
          val env = Line.kr(dur = pdur.ir)
          val done = Done.kr(env)
          done.react(diskDone())
          DiskIn.ar(1, b.id, loop = 1) /* * env */ * pamp.kr
        }
      }.make
     g.control("pos").v_=(rrand(0.0, 1800.0))
     g.control("dur").v_=(exprand(MIN_DUR, MAX_DUR))
     g
   }

   def filter1( implicit tx: ProcTxn ) : Proc = {
      val f = filter(name + "-trans") {
        val pin2 = pAudioIn("in2") // Some( RichBus.audio( Server.default, 1 ))
        val pfade = pAudio("fade", ParamSpec(0, 1), 0)
        graph {
          in1: In =>
          //            val in2        = pin2.ar
          //            require( in1.numOutputs == in2.numOutputs )
          //            val fade       = pfade.ar.linlin( 0, 1, -1, 1 )
          //            val peakTrig   = Impulse.kr( 10 )
          //            val dc         = DC.ar( 1 )
          //            val peak1      = Peak.ar( in1, peakTrig )
          //            val ramp1      = Ramp.ar( peak1, 0.1 )
          //            val dc1        = LinXFade2.ar( ramp1, dc, fade )
          //            val sig1       = LinXFade2.ar( in1, dc1, fade )
          //            val peak2      = Peak.ar( in2, peakTrig )
          //            val ramp2      = Ramp.ar( peak2, 0.1 )
          //            val dc2        = LinXFade2.ar( dc, ramp2, fade )
          //            val sig2       = LinXFade2.ar( dc2, in2, fade )
          //            sig1 * sig2
            val in2 = pin2.ar
            require(in1.numChannels /* .numOutputs */ == in2.numChannels /* .numOutputs */)
            val fade = pfade.ar
            val fade1 = fade.linlin(0, 1.0 / 3, -1, 1).clip2(1)
            val fade2 = fade.linlin(2.0 / 3, 1, 1, -1).clip2(1)
            val fade3 = fade.linlin(1.0 / 3, 2.0 / 3, -1, 1).clip2(1)

            val dlyTime = 0.01
            val dly1 = DelayN.ar(in1, dlyTime, dlyTime)
            val dly2 = DelayN.ar(in2, dlyTime, dlyTime)
            val norm1 = Normalizer.ar(in1, dur = dlyTime) * dly2
            val norm2 = Normalizer.ar(in2, dur = dlyTime) * dly1
            val normFade1 = LinXFade2.ar(dly1, norm1, fade1)
            val normFade2 = LinXFade2.ar(dly2, norm2, fade2)
            val sig = LinXFade2.ar(normFade1, normFade2, fade3)
            sig
        }
      }.make
      f
   }
}
