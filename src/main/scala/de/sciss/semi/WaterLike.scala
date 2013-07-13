/*
 *  WaterLike.scala
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
import SemiNuages._
import de.sciss.synth.proc.{DSL, ProcTxn, Ref, Proc}
import DSL._

object WaterLike {
  case class Channel(idx: Int, procFilter: Proc, procGen: Proc)

  //   protected abstract sealed class Side( val extension: String )
  //   case object Left extends Side( "L" )
  //   case object Right extends Side( "R" )
}

trait WaterLike extends MiddleProcess {
  import WaterLike._

  private val urn = new Urn(0 until NUM_PLATES: _*)

  protected val ch1: Ref[Option[Channel]] = Ref(None)
  protected val ch2: Ref[Option[Channel]] = Ref(None)

  // --- abstract ---
  protected def filters(implicit tx: ProcTxn): (Proc, Proc)
  protected def gens   (implicit tx: ProcTxn): (Proc, Proc)

  protected def start()(implicit tx: ProcTxn) {
    val Seq(idx1, idx2) = urn.take(2)
    //      ch1.set( ch1v )
    //      ch2.set( ch2v )

    //      val cuts0 = cutOut( Vector( 0.0 -> 567.74122 ), 45, 27, 67 )
    //      cutsRef.set( cuts0.tail )
    //      val cut = cuts0.head
    //      val chan1 = createOne( idx1, "L", cut )
    //      val chan2 = createOne( idx2, "R", cut )
    //      ch1.set( Some( chan1 ))
    //      ch2.set( Some( chan2 ))

    val (flt1, flt2) = filters
    val (gen1, gen2) = gens
    val chan1 = Channel(idx1, flt1, gen1)
    ch1.set(Some(chan1))
    val chan2 = Channel(idx2, flt2, gen2)
    ch2.set(Some(chan2))
    val chans = chan1 :: chan2 :: Nil

    chans foreach { ch =>
      val plate = plates(ch.idx)
      val insertTarget = ProcHelper.findOutEdge(plate.collector2, collMaster).get.in
      plate.collector2 ~| ch.procFilter |> insertTarget
      ch.procGen ~> ch.procFilter.audioInput("in2")
      ch.procFilter .bypass
      ch.procGen    .play
      ch.procFilter .play
    }

    chans.zipWithIndex.foreach { case (ch, idx) =>
      xfade(engageFade) {
        ch.procFilter.engage
      }
      val fdt = fadeTime()
      glide(fdt) {
        ch.procFilter.control("fade").v_=(1)
      }
      Analysis.log(s"fade-in-channel $idx ${chans(idx).idx} ${(fdt * 44100L).toLong} $name")
    }
  }

  protected def stop()(implicit tx: ProcTxn) {
    //      ProcHelper.stopAndDispose( )
    val fdt     = fadeTime()
    val chan1O  = ch1.swap(None)
    val chan2O  = ch2.swap(None)
    glide(fdt) {
      (chan1O :: chan2O :: Nil).zipWithIndex.foreach {
        case (Some(ch), idx) =>
          Analysis.log(s"fade-out-channel $idx ${(fdt * 44100L).toLong} $name")
          ch.procFilter.control("fade").v_=(0)
          ProcHelper.whenGlideDone(ch.procFilter, "fade") { implicit tx =>
            ProcHelper.stopAndDispose(engageFade, ch.procFilter, postFun = ch.procGen.dispose(_))
          }
        case _ =>
      }
    }
  }
}