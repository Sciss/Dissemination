/*
 *  Meta.scala
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
import de.sciss.synth.Constant
import de.sciss.synth.proc.{DSL, ProcTxn}
import DSL._

object Meta {
   var verbose = true
}

class Meta {
   import Meta._
   
   private val procs = plates :: regen :: sprenger :: windspiel :: Nil

   def init( implicit tx: ProcTxn ) {
      val ptrig = (diff( "meta" ) {
         graph {
            procs foreach { proc =>
               val tr = proc.trigger
//               proc.offTrigger match {
//                  case Constant( 0 ) =>
//                  case t => t.react {
//                     ProcTxn.spawnAtomic { implicit tx => proc.active = false }
//                  }
//               }
               val onTrig  = tr.max( 0 )
               val offTrig = (-tr).max( 0 )
               onTrig.react {
                  if( verbose ) println( "ACTIVATING " + proc.name )
                  ProcTxn.spawnAtomic { implicit tx => proc.active = true }
               }
               offTrig.react {
                  if( verbose ) println( "DEACTIVATING " + proc.name )
                  ProcTxn.spawnAtomic { implicit tx => proc.active = false }
               }
            }
            0
         }
      }).make
      ptrig.play
   }
}