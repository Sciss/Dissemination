/*
 *  ColorLike.scala
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

import SemiNuages._
import de.sciss.synth.proc.{DSL, Ref, ProcTxn, Proc}
import DSL._

object ColorLike {
  case class Channel(procFilter: Proc, procGen: Proc)
}

trait ColorLike extends MiddleProcess {
  import ColorLike._

  protected val ch      = Ref(Option.empty[Channel])

  // --- abstract ---
  protected def filter1(implicit tx: ProcTxn): Proc
  protected def gen1   (implicit tx: ProcTxn): Proc

  // protected def plate     : Plate
  protected def delayGen  : Boolean

  protected def idx: Int

  // final def nameCh = s"$name-$idx"

  final protected def plate = plates(idx)

  protected def start()( implicit tx: ProcTxn ) {
    val flt   = filter1
    val g     = gen1
    val chan  = Channel(flt, g)
    ch.set(Some(chan))

    val pl = plate
    val insertTarget = ProcHelper.findOutEdge(pl.collector2, collMaster).get.in
    pl.collector2 ~| chan.procFilter |> insertTarget
    //      chan.procGen ~> chan.procFilter.audioInput( "in2" )
    chan.procFilter.bypass

    val pDummy = if (delayGen) {
      factory("@").make
    } else chan.procGen

    pDummy ~> chan.procFilter.audioInput("in2")
    //      chan.procGen.play
    chan.procFilter.play

    //      xfade( engageFade ) {
    //         chan.procFilter.engage
    //      }
    xfade(engageFade) {
      chan.procFilter.engage
    }

    val fdt = fadeTime()
    glide(fdt) {
      chan.procFilter.control("fade").v_=(1)
    }
    Analysis.log(s"fade-in ${(fdt * 44100L).toLong} $name")

    if (delayGen) ProcHelper.whenGlideDone(chan.procFilter, "fade") { implicit tx =>
      chan.procGen ~> chan.procFilter.audioInput("in2")
      chan.procGen.play
      pDummy.dispose
    }
  }

  protected def stop()(implicit tx: ProcTxn) {
    val fdt   = fadeTime()
    val chanO = ch.swap(None)
    glide(fdt) {
      chanO.foreach { ch =>
        ch.procFilter.control("fade").v_=(0)
        ProcHelper.whenGlideDone(ch.procFilter, "fade") { implicit tx =>
          ProcHelper.stopAndDispose(engageFade, ch.procFilter, postFun = ch.procGen.dispose(_))
        }
      }
    }
    Analysis.log(s"fade-out ${(fdt * 44100L).toLong} $name")
  }

  protected def diskDone() {
    ProcTxn.spawnAtomic(implicit tx =>
      active_=(onOff = false)
    )
  }
}