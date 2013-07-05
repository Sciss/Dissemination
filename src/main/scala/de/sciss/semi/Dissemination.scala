/*
 *  Dissemination.scala
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

import de.sciss.synth._
import de.sciss.nuages.{NuagesFrame, NuagesConfig}
import java.awt.{GraphicsEnvironment, EventQueue}
import collection.immutable.{ IndexedSeq => IIdxSeq }
import java.io.{FileOutputStream, FileInputStream, File}
import java.util.Properties
import proc.{ProcTxn, ProcDemiurg}
import actors.Actor
import de.sciss.synth.swing.j.{JNodeTreePanel, JServerStatusPanel}

object Dissemination {
   val fs = File.separator

//   val BASE_PATH           = System.getProperty( "user.home" ) + fs + "Desktop" + fs + "Dissemination"
   val INTERNAL_AUDIO      = false // true
   val NUAGES_ANTIALIAS    = false
   val MASTER_INDEX        = 2
   val SAMPLE_RATE         = 44100.0

   val HEADPHONES_INDEX    = 0

   val OPEN_NUAGES         = true
   val OPEN_NODETREE       = false

   val CMD_KILL_ALL        = "/usr/bin/killall"
   val CMD_OPEN            = "/usr/bin/open"

   private val PROP_BASEPATH  = "basepath"
   private val PROP_SCPATH    = "scpath"
   private val PROP_GRAZ      = "graz"
   private val PROP_STARTMETA = "startmeta"
   private val PROP_MASTERGAIN= "mastergain"

   val properties          = {
      val file = new File( "dissemination-settings.xml" )
      val prop = new Properties()
      if( file.isFile ) {
         val is = new FileInputStream( file )
         prop.loadFromXML( is )
         is.close()
      } else {
         prop.setProperty( PROP_BASEPATH,
            new File( new File( System.getProperty( "user.home" ), "Desktop" ), "Dissemination" ).getAbsolutePath )
         prop.setProperty( PROP_SCPATH,
            new File( new File( new File( new File( new File( System.getProperty( "user.home" ), "Documents" ),
               "devel" ), "SuperCollider3" ), "common" ), "build" ).getAbsolutePath )
         val os = new FileOutputStream( file )
         prop.storeToXML( os, "Dissemination Settings" )
         os.close()
      }
      prop
   }

   val MASTER_GAIN         = properties.getProperty( PROP_MASTERGAIN, "-24" ).toDouble

   val BASE_PATH           = properties.getProperty( PROP_BASEPATH )
   val RECORD_PATH         = BASE_PATH + fs + "rec"
   val AUDIO_PATH          = BASE_PATH + fs + "audio_work" 
   val WORK_PATH           = AUDIO_PATH + fs + "work"
   val INJECT_PATH         = BASE_PATH + fs + "inject"
   val TEMP_PATH           = System.getProperty( "java.io.tmpdir" ) // BASE_PATH + fs + "tmp"

   val GRAZ                = properties.getProperty( PROP_GRAZ, "false" ).toBoolean // false
   val START_META          = properties.getProperty( PROP_STARTMETA, "true" ).toBoolean

   val NUM_PLATES          = if( GRAZ ) 7 else 5
//   val START_WITH_TRANSIT  = false // GRAZ
   val PLATE_TRANSITS      = if( GRAZ ) {
      IIdxSeq.tabulate( NUM_PLATES )( i => i < 2 || (NUM_PLATES - 1 - i) < 2 )  
   } else {
      IIdxSeq.tabulate( NUM_PLATES )( i => !(i % 2 == 0) ) // START_WITH_TRANSIT
   }
   val MASTER_NUMCHANNELS  = if( INTERNAL_AUDIO ) 2 else NUM_PLATES

   val CMD_SCSYNTH         = properties.getProperty( PROP_SCPATH ) + fs + "scsynth"
   
   val options          = {
      val o = Server.Config()
      if( INTERNAL_AUDIO ) {
         o.deviceNames        = Some( "Built-in Microphone" -> "Built-in Output" )
      } else {
         o.deviceName         = Some( "MOTU 828mk2" )
      }
      o.programPath        = CMD_SCSYNTH // properties.getProperty( PROP_SCPATH ) + fs + "scsynth"
      o.inputBusChannels   = 10
      o.outputBusChannels  = 10
      o.audioBusChannels   = 512
      o.loadSynthDefs      = false
      o.memorySize         = 65536
      o.zeroConf           = false
//      o.port               = 0
//      o.transport          = TCP
//      o.programPath        = properties.getProperty( PROP_SCPATH ) + fs + "scsynth"
      o.build
   }

   lazy val SCREEN_BOUNDS =
         GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getDefaultConfiguration.getBounds
   
   val support = new REPLSupport
   var masterBus : AudioBus = null
   var headphonesBus : AudioBus = null

   var gui: GUI            = null

   @volatile var s: Server       = _
   @volatile var booting: ServerConnection = _
   @volatile var config: NuagesConfig = _

   def main( args: Array[ String ]) {
      guiRun { init() }
   }

   def guiRun( code: => Unit ) {
      EventQueue.invokeLater( new Runnable { def run() { code }})
   }

  def init() {
    // prevent actor starvation!!!
    // --> http://scala-programming-language.1934581.n4.nabble.com/Scala-Actors-Starvation-td2281657.html
    System.setProperty("actors.enableForkJoin", "false")

    val sif   = new ScalaInterpreterFrame( support /* ntp */ )
    val ssp   = new JServerStatusPanel()
    val sspw  = ssp.makeWindow
    val ntp   = if (OPEN_NODETREE) {
      val res   = new JNodeTreePanel()
      val ntpw  = res.makeWindow()
      ntpw.setLocation(sspw.getX, sspw.getY + sspw.getHeight + 32)
      ntpw.setVisible(true)
      Some(res)
    } else None
    gui = new GUI
    sspw.setVisible(true)
    sif.setLocation(sspw.getX + sspw.getWidth + 32, sif.getY)
    sif.setVisible(true)
    booting = Server.boot(config = options) {
      case ServerConnection.Preparing(srv) =>
        ssp.server = Some(srv)
        ntp.foreach(_.group = Some(srv.rootNode))

      case ServerConnection.Running(srv) =>
        ProcDemiurg.addServer(srv)
        s = srv
        support.s = srv

        //            if( DUMP_OSC ) s.dumpOSC(1)

        // nuages
        initNuages()
        //            new GUI
    }
    Runtime.getRuntime.addShutdownHook(new Thread {
      override def run() {
        shutDown()
      }
    })
    //      booting.start
  }

  private def initNuages() {
    masterBus = if (INTERNAL_AUDIO) {
      new AudioBus(s, 0, 2)
    } else {
      new AudioBus(s, MASTER_INDEX, MASTER_NUMCHANNELS)
    }
    //      val soloBus    = Bus.audio( s, 2 )
    headphonesBus     = new AudioBus(s, HEADPHONES_INDEX, 2)
    val masterChans   = Vector.tabulate(masterBus.numChannels)(_ + masterBus.index)
    config            = NuagesConfig(s, Some(masterChans), None, Some(RECORD_PATH))
    if (OPEN_NUAGES) {
      val f = new NuagesFrame(config)
      f.panel.display.setHighQuality(NUAGES_ANTIALIAS)
      f.setSize(640, 480)
      f.setLocation(((SCREEN_BOUNDS.width - f.getWidth) >> 1) + 100, 10)
      f.setVisible(true)
      support.nuages = f
    }

    Actor.actor {
      ProcTxn.atomic { implicit tx =>
        SemiNuages.init(s)
      }
      if (START_META) ProcTxn.atomic { implicit tx =>
        SemiNuages.meta.init()
      }
    }
  }

  def quit() { sys.exit() }

  def shutDownComputer() {
    val pb = new ProcessBuilder("/bin/sh", BASE_PATH + fs + "shutdown.sh")
    pb.start()
  }

  private def shutDown() {
    // sync.synchronized { }
    if (s != null && s.condition != Server.Offline) {
      s.quit()
      s = null
    }
    if (booting != null) {
      booting.abort()
      booting = null
    }
  }
}