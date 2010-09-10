/*
 *  ColorLike.scala
 *  (Dissemination)
 *
 *  Copyright (c) 2010 Hanns Holger Rutz. All rights reserved.
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
import Util._

object ColorLike {
   case class Channel( procFilter: Proc, procGen: Proc )
}

trait ColorLike extends SemiProcess {
   import ColorLike._
   
   protected val ch: Ref[ Option[ Channel ]] = Ref( None )
   private val activeRef = Ref( false )

   // --- abstract ---
   protected def filter( implicit tx: ProcTxn ) : Proc
   protected def gen( implicit tx: ProcTxn ) : Proc
   protected def plate : Plate
   protected def minFade  : Double
   protected def maxFade : Double
   protected def engageFade : Double

   def active( implicit tx: ProcTxn ) = activeRef()
   def active_=( onOff: Boolean )( implicit tx: ProcTxn ) {
      val wasActive = activeRef.swap( onOff )
      if( wasActive == onOff ) return
      if( onOff ) start else stop
   }

   private def start( implicit tx: ProcTxn ) {
      val flt  = filter
      val g    = gen
      val chan = Channel( flt, gen )
      ch.set( Some( chan ))

      val pl = plate
      val insertTarget = ProcHelper.findOutEdge( pl.collector2, collMaster ).get.in
      pl.collector2 ~| chan.procFilter |> insertTarget
      chan.procGen ~> chan.procFilter.audioInput( "in2" )
      chan.procFilter.bypass
      chan.procGen.play
      chan.procFilter.play

      xfade( engageFade ) {
         chan.procFilter.engage
      }
      glide( exprand( minFade, maxFade )) {
         chan.procFilter.control( "fade" ).v = 1
      }
   }

   private def stop( implicit tx: ProcTxn ) {
      val fdt = exprand( minFade, maxFade )
      val chanO = ch.swap( None )
      glide( fdt ) {
         chanO.foreach { ch =>
            ch.procFilter.control( "fade" ).v = 0
            ProcHelper.whenGlideDone( ch.procFilter, "fade" ) { implicit tx =>
               ProcHelper.stopAndDispose( engageFade, ch.procFilter, postFun = ch.procGen.dispose( _ ))
            }
         }
      }
   }
}