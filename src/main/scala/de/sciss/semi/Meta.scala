/*
 *  Meta.scala
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
import de.sciss.synth.Constant
import de.sciss.synth.proc.{Proc, DSL, ProcTxn}
import collection.breakOut
import DSL._
import java.util.TimerTask
import Dissemination._

object Meta {
   var verbose       = false
   val AUTO_RESTART  = (2.4 * 60 * 60 * 1000L).toLong   // each three hours XXX now: 2.4 hours
}

class Meta {
  import Meta._

  private val procs = plates :: regen :: sprenger :: windspiel :: apfel :: phylet :: zeven :: licht ::
    helicopter.map(List(_)).getOrElse(Nil)
  // XXX scherben
  private val procMap: Map[String, SemiProcess] = procs.map(p => p.name -> p)(breakOut)

  def init()(implicit tx: ProcTxn) {
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
                  ProcTxn.spawnAtomic { implicit tx =>
                     if( !proc.exclusives.exists( name =>
                        procMap.get( name ).map( _.active ).getOrElse( false )
                     )) {
                        proc.active = true
                     }
                  }
               }
               offTrig.react {
                  if( verbose ) println( "DEACTIVATING " + proc.name )
                  ProcTxn.spawnAtomic { implicit tx =>
//                     if( procs.exists( p => p != proc && p.active ))
                     proc.active = false
                  }
               }
            }
            0
         }
      }).make
      ptrig.play

      val tim = new java.util.Timer()
      tim.schedule( new TimerTask {
         def run { ProcTxn.atomic { implicit tx => xfade( 15 ) { pMaster.stop }}}
      }, AUTO_RESTART )
      tim.schedule( new TimerTask {
         def run {
            val pb = new ProcessBuilder( "/bin/sh", BASE_PATH + fs + "RestartDissem.sh" )
            pb.start()
         }
      }, AUTO_RESTART + 16000L )
   }
}