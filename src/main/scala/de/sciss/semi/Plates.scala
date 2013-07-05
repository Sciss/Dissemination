/*
 *  Plates.scala
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
import de.sciss.synth.proc.ProcTxn
import collection.immutable.{ IndexedSeq => IIdxSeq }
import de.sciss.synth.GE
import de.sciss.synth.ugen.{TDuty, Dwhite, Dseq}
import de.sciss.synth

object Plates {
  val MIN_ON_DUR  =  60.0
  val MAX_ON_DUR  = 120.0
  val MIN_OFF_DUR =  60.0
  val MAX_OFF_DUR = 120.0

  def create(implicit tx: ProcTxn): Plates = {
    val res = new Plates
    res.init
    res
  }
}

class Plates private() extends SemiProcess {
   import Plates._

   def name = "plates"
   def exclusives = Set.empty[ String ]
   def trigger : GE = {
      import synth._
      TDuty.kr( Dseq( Dwhite( MIN_ON_DUR, MAX_ON_DUR ) :: Dwhite(  MIN_OFF_DUR, MAX_OFF_DUR ) :: Nil, inf ), Dseq( 1 :: -1 :: Nil, inf ))
   }

   private var plates: IIdxSeq[ Plate ] = _

   private def init( implicit tx: ProcTxn ) {
      plates = PLATE_TRANSITS.zipWithIndex map { tup =>
         val (transit, idx) = tup
         Plate( idx, transit )
      }
      plates.foreach( p => p.initNeighbours(
         { val i = p.id - 2; if( i >= 0 ) Some( plates( i )) else None },
         { val i = p.id + 2; if( i < NUM_PLATES ) Some( plates( i )) else None }))
   }

   def active( implicit tx: ProcTxn ) : Boolean = plates( 0 ).active
   def active_=( onOff: Boolean )( implicit tx: ProcTxn ) { plates.foreach( _.active_=(onOff) )}

   def apply( idx: Int ) = plates( idx )
}