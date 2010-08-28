/*
 *  Dissemination.scala
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

import javax.swing.{WindowConstants, JFrame}
import java.io.File
import de.sciss.synth.swing.{ServerStatusPanel, NodeTreePanel}
import de.sciss.synth.proc.ProcDemiurg
import de.sciss.synth._
import de.sciss.nuages.{NuagesFrame, NuagesConfig}
import java.awt.{GraphicsEnvironment, EventQueue}
import collection.immutable.{ IndexedSeq => IIdxSeq }

object Dissemination {
   val GRAZ                = false

   val NUM_PLATES          = if( GRAZ ) 7 else 5
   val START_WITH_TRANSIT  = GRAZ
   val BASE_PATH           = System.getProperty( "user.home" ) + File.separator + "Desktop" + File.separator + "Dissemination"
   val INTERNAL_AUDIO      = true
   val NUAGES_ANTIALIAS    = false
   val MASTER_OFFSET       = 0

   val PLATE_TRANSITS      = IIdxSeq.tabulate( NUM_PLATES )( i => ((i % 2) == 0) == START_WITH_TRANSIT )
   val MASTER_NUMCHANNELS  = if( INTERNAL_AUDIO ) 2 else NUM_PLATES

   val options          = {
      val o = new ServerOptionsBuilder()
      if( INTERNAL_AUDIO ) {
         o.deviceNames        = Some( "Built-in Microphone" -> "Built-in Output" )
      } else {
         o.deviceName         = Some( "MOTU 828mk2" )
      }
      o.inputBusChannels   = 10
      o.outputBusChannels  = 10
      o.audioBusChannels   = 512
      o.loadSynthDefs      = false
      o.memorySize         = 65536
      o.zeroConf           = false
//      o.programPath        = properties.getProperty( PROP_SCPATH ) + File.separator + "scsynth"
      o.build
   }

   lazy val SCREEN_BOUNDS =
         GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getDefaultConfiguration.getBounds
   
   val support = new REPLSupport
   var masterBus : AudioBus = null

   @volatile var s: Server       = _
   @volatile var booting: ServerConnection = _
   @volatile var config: NuagesConfig = _

   def main( args: Array[ String ]) {
      guiRun { init }
   }

   def guiRun( code: => Unit ) {
      EventQueue.invokeLater( new Runnable { def run = code })
   }

   def init {
      // prevent actor starvation!!!
      // --> http://scala-programming-language.1934581.n4.nabble.com/Scala-Actors-Starvation-td2281657.html
      System.setProperty( "actors.enableForkJoin", "false" )

      val sif  = new ScalaInterpreterFrame( support /* ntp */ )
      val ssp  = new ServerStatusPanel()
      val sspw = ssp.makeWindow
      val ntp  = new NodeTreePanel()
      val ntpw = ntp.makeWindow
      ntpw.setLocation( sspw.getX, sspw.getY + sspw.getHeight + 32 )
      sspw.setVisible( true )
      ntpw.setVisible( true )
      sif.setLocation( sspw.getX + sspw.getWidth + 32, sif.getY )
      sif.setVisible( true )
      booting = Server.boot( options = options )
      booting.addListener {
         case ServerConnection.Preparing( srv ) => {
            ssp.server = Some( srv )
            ntp.server = Some( srv )
         }
         case ServerConnection.Running( srv ) => {
            ProcDemiurg.addServer( srv )
            s = srv
            support.s = srv

//            if( DUMP_OSC ) s.dumpOSC(1)

            // nuages
            initNuages
//            new GUI
         }
      }
      Runtime.getRuntime().addShutdownHook( new Thread { override def run = shutDown })
      booting.start
   }

   private def initNuages {
      masterBus  = if( INTERNAL_AUDIO ) {
         new AudioBus( s, 0, 2 )
      } else {
         new AudioBus( s, MASTER_OFFSET, MASTER_NUMCHANNELS )
      }
      val soloBus    = Bus.audio( s, 2 )
      val recordPath = BASE_PATH + "rec"
      config         = NuagesConfig( s, Some( masterBus ), Some( soloBus ), Some( recordPath ))
      val f          = new NuagesFrame( config )
      f.panel.display.setHighQuality( NUAGES_ANTIALIAS )
      f.setSize( 640, 480 )
      f.setLocation( ((SCREEN_BOUNDS.width - f.getWidth()) >> 1) + 100, 10 )
      f.setVisible( true )
      support.nuages = f
      SemiNuages.init( s, f )
   }

   def quit { System.exit( 0 )}

   private def shutDown { // sync.synchronized { }
      if( (s != null) && (s.condition != Server.Offline) ) {
         s.quit
         s = null
      }
      if( booting != null ) {
         booting.abort
         booting = null
      }
    }
}