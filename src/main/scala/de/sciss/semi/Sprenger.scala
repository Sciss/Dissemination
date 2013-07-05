/*
 *  Sprenger.scala
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
import Util._
import de.sciss.synth
import synth.GE
import synth.proc.{Proc, ExpWarp, DSL, ParamSpec, ProcTxn, Ref}
import collection.immutable.{ IndexedSeq => IIdxSeq }
import DSL._
import annotation.tailrec
import synth.ugen.Dust

object Sprenger {
   val FADE_DUR   = 6.0
   val MIN_KEEP   = 45.0
   val MIN_SKIP   = 27.0
   val MAX_SKIP   = 67.0
   val INOUT_MIN  = 16.0
   val INOUT_MAX  = 48.0
   val SHORT_FADE = 3.0
}

class Sprenger extends WaterLike {
  import Sprenger._

  private val cutsRef = Ref(Seq.empty[(Double, Double)])

  protected def minFade     = INOUT_MIN
  protected def maxFade     = INOUT_MAX
  protected def engageFade  = SHORT_FADE

  def name        = "sprenger"
  def exclusives  = Set("regen")

  def trigger: GE = {
    // XXX
    import synth._
    Dust.kr(1.0 / 360)
  }

  protected def filters(implicit tx: ProcTxn): (Proc, Proc) = {
    val idx   = rrand(1, 2)
    val fact  = factory("water-trans" + idx)
    Analysis.log(s"$name-filters idx $idx")
    (fact.make, fact.make)
  }

  // produces a scrambled cut-up of the given pieces
  @tailrec
  private def cutOut(pieces: IIdxSeq[(Double, Double)], minKeep: Double, minSkip: Double, maxSkip: Double): IIdxSeq[(Double, Double)] = {
    val minPcsLen = minKeep + minKeep + minSkip
    val f         = pieces.filter(tup => tup._2 - tup._1 >= minPcsLen)
    if (f.size == 0) return pieces
    val piece     = f(rand(f.size))
    val idx       = pieces.indexOf(piece)
    val pieceDur  = piece._2 - piece._1
    val cutDur    = exprand(minSkip, math.min(pieceDur - (minKeep + minKeep), maxSkip))
    val cutPos    = exprand(minKeep, pieceDur - (minKeep + cutDur))
    val pc1       = piece._1 -> (piece._1 + cutPos)
    val pc2       = piece._1 + cutPos + cutDur -> piece._2
    cutOut(pieces.patch(idx, pc1 :: pc2 :: Nil, 1), MIN_KEEP, MIN_SKIP, MAX_SKIP)
  }

  protected def gens(implicit tx: ProcTxn): (Proc, Proc) = {
    import synth._
    import ugen._

    val cuts0 = cutOut(pieces = Vector(0.0 -> 567.74122), minKeep = 45, minSkip = 27, maxSkip = 67)
    // store the tail for chaining, and begin with head cut
    cutsRef.set(cuts0.tail)
    val cut = cuts0.head

    def map(ext: String): Proc = {
         val g = gen(name + "-" + ext) {
           val pamp = pControl("amp", ParamSpec(0.dbamp, 18.dbamp, ExpWarp), 15.dbamp)
           val ppos = pScalar("pos", ParamSpec(0, 600), 1)
           val pdur = pScalar("dur", ParamSpec(1, 600), 1)
           graph {
             val path = AUDIO_PATH + fs + "080304_173812_MisionSprengerOKM-" + ext + ".aif"
             val startFrame = (ppos.v * 44100L).toLong // AudioFileCache.spec( path ).numFrames
             val b = bufCue(path, startFrame)
             val done = Done.kr(Line.kr(dur = pdur.ir - FADE_DUR))
             done.react {
               cutDone(ext)
             }
             DiskIn.ar(1, b.id) * pamp.kr
           }
         }.make
         setCut( g, cut )
         g
      }
     val gen1 = map("L")
     val gen2 = map("R")
      (gen1, gen2)
   }

   private def cutDone( ext: String ) {
      if( ext == "L" ) {
         ProcTxn.spawnAtomic( implicit tx => if( active ) {
            val cuts0 = cutsRef()
            if( cuts0.nonEmpty ) {
               xfade( FADE_DUR ) {
                  val cut  = cuts0.head
                  val cuts = cuts0.tail
                  cutsRef.set( cuts )
                  ch1().foreach( ch => setCut( ch.procGen, cut ))
                  ch2().foreach( ch => setCut( ch.procGen, cut ))
               }
            } else {
               active_=(onOff = false)
            }
         })
      }
   }

  private def setCut(p: Proc, cut: (Double, Double))(implicit tx: ProcTxn) {
    val (pos, end) = cut
    val dur = end - pos

    p.control("pos").v_=(pos)
    p.control("dur").v_=(dur)

    Analysis.log(s"$name-gen-cut pos $pos dur $dur")
  }
}