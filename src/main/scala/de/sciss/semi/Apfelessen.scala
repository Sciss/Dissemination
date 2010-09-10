/*
 *  Apfelessen.scala
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

import de.sciss.synth
import synth.proc.{Proc, ProcTxn}
import synth.ugen.Dust
import synth.GE
import Util._
import SemiNuages._

object Apfelessen {
//   val GEN_FADE_IN      = 0.1
//   val GEN_FADE_IN      = 0.1
   val MIN_FILTER_FADE  = 10
   val MAX_FILTER_FADE  = 10 
}

class Apfelessen( idx: Int ) extends ColorLike {
   import Apfelessen._
   
   protected def minFade      = MIN_FILTER_FADE
   protected def maxFade      = MAX_FILTER_FADE
   protected def engageFade   = 1.0

   def name = "apfel"
   def exclusives = Set.empty[ String ] // Set( "regen" )
   def trigger : GE = { // XXX
      import synth._
      Dust.kr( 1.0 / 360 )
   }

   def plate = plates( idx )

   def gen( implicit tx: ProcTxn ) : Proc = error( "NOT YET IMPLEMENTED" )
   def filter( implicit tx: ProcTxn ) : Proc = error( "NOT YET IMPLEMENTED" )
}