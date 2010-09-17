/*
 *  BounceSynthContext.scala
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

import scala.collection.immutable.{ Queue => IQueue }
import scala.collection.mutable.{ PriorityQueue }
import java.io.{ BufferedInputStream, BufferedReader, File, InputStreamReader, IOException, RandomAccessFile }
import java.nio.{ ByteBuffer }
import javax.swing.{ SwingWorker }
import scala.math._
import de.sciss.osc.{ OSCBundle, OSCMessage, OSCPacket, OSCPacketCodec }
import de.sciss.synth._
import Dissemination._

object BounceSynthContext {
   @throws( classOf[ IOException ])
   def apply( so: ServerOptionsBuilder ) : BounceSynthContext = {
      val oscPath = File.createTempFile( "kontur", ".osc", new File( TEMP_PATH ))
      val oscFile = new RandomAccessFile( oscPath, "rw" )
      so.nrtCommandPath = oscPath.getCanonicalPath
      val context = new BounceSynthContext( so.build, oscPath, oscFile )
      context
   }

   private var current: BounceSynthContext = null

   trait AbstractBundle {
      protected var msgs = IQueue[ OSCMessage ]()

      @throws( classOf[ IOException ])
      def send: Unit

      def add( msg: OSCMessage ) {
         msgs = msgs.enqueue( msg )
      }

      def messages: Seq[ OSCMessage ] = msgs
   }
}

class BounceSynthContext private( so: ServerOptions, oscPath: File, oscFile: RandomAccessFile ) {
   import BounceSynthContext._

   private val verbose =  false

   private var timebaseVar = 0.0
   private val bundleQueue = new PriorityQueue[ Bundle ]()( BundleOrdering )
   private var fileOpen    = true
   private val codec       = new OSCPacketCodec( OSCPacketCodec.MODE_GRACEFUL )
   private val bb          = ByteBuffer.allocateDirect( 65536 )
   private val fch         = oscFile.getChannel()

   protected var bundle: AbstractBundle = null
   
   def timebase = timebaseVar
   def timebase_=( newVal: Double ) {
      if( newVal < timebaseVar ) throw new IllegalArgumentException( newVal.toString )
      if( newVal > timebaseVar ) {
         advanceTo( newVal )
      }
   }

   def perform( thunk: => Unit ) {
      perform( thunk, -1 )
   }

   def delayed( tb: Double, delay: Double )( thunk: => Unit ) {
      perform( thunk, (tb - timebase) + delay )
   }

   private def perform( thunk: => Unit, time: Double ) {
      val savedContext  = BounceSynthContext.current
      val savedBundle   = bundle
      try {
         initBundle( time )
         BounceSynthContext.current = this
         thunk
         sendBundle
      }
      finally {
         BounceSynthContext.current = savedContext
         bundle = savedBundle
      }
   }

   private def sendBundle {
      try {
         bundle.send // sendBundle( bundle )
      }
      catch { case e: IOException => e.printStackTrace }
      finally {
         bundle = null
      }
   }

   def sampleRate : Double = so.sampleRate

   private def advanceTo( newTime: Double ) {
      var keepGoing = true
      while( keepGoing ) {
         keepGoing = bundleQueue.headOption.map( b => {
            if( b.time <= newTime ) {
               bundleQueue.dequeue
               timebaseVar = b.time // important because write calls doAsync
               write( b )
               true
            } else false
         }) getOrElse false
      }
      timebaseVar = newTime
   }

   protected def initBundle( delta: Double ) {
      bundle = new Bundle( timebase + max( 0.0, delta ))
   }

   @throws( classOf[ IOException ])
   def render: SwingWorker[ _, _ ] = {
      flush
      close

      val dur = timebaseVar // in seconds
      val program = so.programPath
//      println( "Booting '" + program + "'" )
      val appPath = new File( program )
      // -N cmd-filename input-filename output-filename sample-rate header-format sample-format -o numOutChans
//      server.options.nrtOutputPath.value = descr.file.getCanonicalPath
      val processArgs = so.toNonRealtimeArgs.toArray

//      -N <cmd-filename> <input-filename> <output-filename> <sample-rate> <header-format> <sample-format> 	<...other scsynth arguments>
      
      if( verbose ) println( processArgs.mkString( " " ))
      val pb = new ProcessBuilder( processArgs: _* )
        .directory( appPath.getParentFile )
        .redirectErrorStream( true )

      val w = new SwingWorker[ Int, Unit ]() {
         main =>
         override def doInBackground: Int = {
            var pRunning   = true
            val p          = pb.start
//            val inStream	= new BufferedInputStream( p.getInputStream )
            val inReader = new BufferedReader( new InputStreamReader( p.getInputStream ))
            val printWorker   = new SwingWorker[ Unit, Unit ]() {
               override def doInBackground {
                  try {
                     var lastProg = 0
                     while( true ) {
                        val line = inReader.readLine
                        if( line.startsWith( "nextOSCPacket" )) {
                           val time = line.substring( 14 ).toFloat
                           val prog = (time / dur * 100).toInt
//println( "time = " + time + "; dur = " + dur + "; prog = " + prog )
                           if( prog != lastProg ) {
//                        setProgress( prog )
                              // NOTE: main.setProgress does not work, probably because
                              // the thread is blocking...
                              main.firePropertyChange( "progress", lastProg, prog )
                              lastProg = prog
                           }
                        } else {
                           if( verbose ) System.out.println( line )
                        }
                     }
                  } catch { case e: IOException => }
               }
            }
            try {
               printWorker.execute()
               p.waitFor()
            } catch { case e: InterruptedException => }

            printWorker.cancel( true )

            try {
               val resultCode	= p.exitValue
               if( verbose ) println( "scsynth terminated (" + resultCode +")" )
               resultCode
            }
            catch { case e: IllegalThreadStateException => -1 } // gets thrown if we call exitValue() while sc still running
         }
      }
//    w.execute()
      w
   }

   @throws( classOf[ IOException ])
   private def close {
      if( fileOpen ) {
         oscFile.close
         fileOpen = false
      }
   }

   def dispose {
      try {
         close
         if( !oscPath.delete ) oscPath.deleteOnExit()
      }
      catch { case e: IOException => e.printStackTrace }
   }

   @throws( classOf[ IOException ])
   private def write( b: Bundle ) {
      val bndl = OSCBundle.secs( b.time, b.messages: _* )
      bb.clear
      bndl.encode( codec, bb )
      bb.flip
      oscFile.writeInt( bb.limit() )   // a little bit strange to use both RandomAccessFile...
      fch.write( bb )                  // ...and FileChannel... but neither has both writeInt and write( bb )
      if( verbose ) {
         OSCPacket.printTextOn( codec, System.out, bndl )
      }
//      if( b.hasAsync ) perform { b.doAsync } // important to check hasAsync, as we create an infinite loop otherwise
   }

   @throws( classOf[ IOException ])
   private def enqueue( b: Bundle ) {
      if( b.time < timebase ) throw new IOException( "Negative bundle time" )
      if( b.time == timebase ) {
         write( b )
      } else {
         bundleQueue.enqueue( b )
      }
   }

   @throws( classOf[ IOException ])
   private def flush {
      while( bundleQueue.nonEmpty ) {
         val b = bundleQueue.dequeue
         if( b.time <= timebaseVar ) {
            timebaseVar = b.time // important because write calls doAsync
            write( b )
         }
      }
   }

   def add( msg: OSCMessage ) {
      bundle.add( msg )
   }

   private class Bundle( val time: Double )
   extends AbstractBundle
   {
      @throws( classOf[ IOException ])
      def send {
         enqueue( this )
      }
   }

   private object BundleOrdering extends Ordering[ Bundle ] {
      def compare( x: Bundle, y: Bundle ) : Int = -Ordering.Double.compare( x.time, y.time ) // low times first
   }
}