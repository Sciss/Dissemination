package de.sciss.semi

import scala.collection.immutable.{ Queue => IQueue }
import scala.collection.mutable.{ PriorityQueue }
import java.io.{ BufferedInputStream, BufferedReader, File, InputStreamReader, IOException, RandomAccessFile }
import java.nio.{ ByteBuffer }
import javax.swing.{ SwingWorker }
import scala.math._
import de.sciss.osc.{ OSCBundle, OSCMessage, OSCPacket, OSCPacketCodec }
import de.sciss.synth._

object BounceSynthContext {
   @throws( classOf[ IOException ])
   def apply( so: ServerOptionsBuilder ) : BounceSynthContext = {
//      val appPath = audioPrefs.get( PrefsUtil.KEY_SUPERCOLLIDERAPP, null )
//      if( appPath == null ) {
//         throw new IOException( AbstractApplication.getApplication.getResourceString( "errSCSynthAppNotFound" ))
//      }
//      so.program.value = appPath
//      val so = new ServerOptions()
//      so.blockSize = 1
//      so.sampleRate
      val oscPath = File.createTempFile( "kontur", ".osc" )
      val oscFile = new RandomAccessFile( oscPath, "rw" )
//      oscFile.setLength( 0L )
      so.nrtCommandPath = oscPath.getCanonicalPath
//      val s = new Server( "Bounce", so.build )
      val context = new BounceSynthContext( so.build, oscPath, oscFile )
      context
   }
}

class BounceSynthContext private( so: ServerOptions, oscPath: File, oscFile: RandomAccessFile ) {
   private val verbose =  false

   private var timebaseVar = 0.0
   private val bundleQueue = new PriorityQueue[ Bundle ]()( BundleOrdering )
   private var fileOpen    = true
   private val codec       = new OSCPacketCodec( OSCPacketCodec.MODE_GRACEFUL )
   private val bb          = ByteBuffer.allocateDirect( 65536 )
   private val fch         = oscFile.getChannel()

   protected var bundle: AbstractBundle = null
   
//   // ---- constructor ----
//   {
//      // XXX initTree missing at the moment
//      perform {
//         add( server.defaultGroup.newMsg( server.rootNode, addToHead ))
//      }
//   }

   def timebase = timebaseVar
   def timebase_=( newVal: Double ) {
      if( newVal < timebaseVar ) throw new IllegalArgumentException( newVal.toString )
      if( newVal > timebaseVar ) {
         advanceTo( newVal )
      }
   }

//   override val sampleRate : Double = server.options.sampleRate.value

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
      
      println( processArgs.mkString( " " ))
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
                           System.out.println( line )
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
               println( "scsynth terminated (" + resultCode +")" )
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

//   override def endsAfter( rn: RichNode, dur: Double ) {
////println( "endsAfter " + dur + ": " + rn.node )
//      delayed( timebase, dur ) {
////println( "endsAfter really " + dur + ": " + rn.node )
////         add( rn.node.freeMsg )
////         rn.isOnline = false
//         addAsync( new AsyncAction {
//            def asyncDone { rn.isOnline = false }
//         })
//      } // simulate n_end
//   }

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

   trait AbstractBundle {
      protected var msgs     = IQueue[ OSCMessage ]()
//      protected var asyncs   = Queue[ AsyncAction ]()
//      private var hasAsyncVar = false

      @throws( classOf[ IOException ])
      def send: Unit

      def add( msg: OSCMessage ) {
         msgs = msgs.enqueue( msg )
      }

//      def addAsync( msg: OSCMessage ) {
//         hasAsyncVar = true
//         add( msg )
//      }
//
//      def addAsync( msg: OSCMessage, async: AsyncAction ) {
//         addAsync( async )
//         add( msg )
//      }
//
//      def addAsync( async: AsyncAction ) {
//          hasAsyncVar = true
//          asyncs   = asyncs.enqueue( async )
//       }

//      def hasAsync = hasAsyncVar

      def messages: Seq[ OSCMessage ] = msgs

//      def doAsync {
//         asyncs.foreach( _.asyncDone )
//      }
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