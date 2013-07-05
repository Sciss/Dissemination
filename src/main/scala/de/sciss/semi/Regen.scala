/*
 *  Regen.scala
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

import Dissemination._
import de.sciss.synth
import synth.GE
import synth.proc.{ExpWarp, ParamSpec, DSL, ProcTxn, Proc}
import DSL._
import synth.ugen.Dust
import Util._

object Regen {
  val INOUT_MIN  = 32.0
  val INOUT_MAX  = 64.0
  val SHORT_FADE = 3.0
  val MIN_DUR    = 3 * 60.0
  val MAX_DUR    = 5 * 60.0
}

class Regen extends WaterLike {
  import Regen._

  protected def minFade      = INOUT_MIN
  protected def maxFade      = INOUT_MAX
  protected def engageFade   = SHORT_FADE

  protected def filters(implicit tx: ProcTxn): (Proc, Proc) = {
    val idx   = rrand(1, 2)
    val fact  = factory("water-trans" + idx)
    Analysis.log(s"$name-filters idx $idx")
    (fact.make, fact.make)
  }

  def name = "regen"

  def exclusives = Set("sprenger")

  def trigger: GE = {
    // XXX
    import synth._
    Dust.kr(1.0 / 360)
  }

  protected def gens(implicit tx: ProcTxn): (Proc, Proc) = {
    import synth._
    import ugen._

    def map(ext: String): Proc = {
      val g = gen(name + "-" + ext) {
        val pamp = pControl("amp", ParamSpec(0.dbamp, 18.dbamp, ExpWarp), 3.dbamp)
        val ppos = pScalar("pos", ParamSpec(0, 900), 1)
        val pdur = pScalar("dur", ParamSpec(1, 600), 1)
        graph {
          val path = AUDIO_PATH + fs + "080227_WeimarRegenFensterAT-" + ext + ".aif"
          val startFrame = (ppos.v * 44100L).toLong // AudioFileCache.spec( path ).numFrames
          val b = bufCue(path, startFrame)
          val disk = DiskIn.ar(1, b.id) * pamp.kr
          // XXX DiskIn does _not_ set a done flag!!
          //               val done       = Done.kr( disk )
          val done = DetectSilence.ar(disk, dur = 1.0) +
            Done.kr(Line.kr(dur = pdur.ir))
          done.react {
            diskDone(ext)
          }
          disk
        }
      }.make
      val pos = rrand(0.0, 300.0)
      val dur = exprand(MIN_DUR, MAX_DUR)
      g.control("pos").v_=(pos)
      g.control("dur").v_=(dur)

      Analysis.log(s"$name-gen-$ext pos ${(pos * 44100L).toLong} dur ${(dur * 44100L).toLong}")
      g
    }
    val gen1 = map("L")
    val gen2 = map("R")
    (gen1, gen2)
  }

  private def diskDone(ext: String) {
    if (ext == "L") {
      ProcTxn.spawnAtomic(implicit tx => active_=(onOff = false))
    }
  }
}