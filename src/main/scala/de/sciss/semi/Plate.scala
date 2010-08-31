package de.sciss.semi

import de.sciss.synth._
import collection.immutable.{ IndexedSeq => IIdxSeq }
import proc._
import DSL._
import ugen._
import java.io.File
import Util._

object Plate {
   val verbose = true
   
   val MIN_LOUDNESS_THRESH = 1.0
   val MIN_LOUDNESS_COUNT  = 10     

//   def createFactories( implicit tx: ProcTxn ) {
//      import synth._
//      import ugen._
//
//      gen( "@" ) { graph { Silent.ar( 1 )}}
//
//      diff( "ana" ) {
//         val pid = pScalar( "id", ParamSpec( 0, NUM_PLATES - 1, step = 1 ))
//
//         graph { in =>
//            val bufID   = bufEmpty( 1024 ).id
//            val chain   = FFT( bufID, Mix( in ))
//            val loud    = Loudness.kr( chain )
//            val centr   = SpecCentroid.kr( chain )
//            val flat0    = SpecFlatness.kr( chain )
//            val flatc   = CheckBadValues.kr( flat0, post = 0 )
//            val flat    = Gate.kr( flat0, flatc === 0 )
////          List( loud, centr, flat ).poll( 2 )
//            val compound= List( loud, centr, flat )
//            val smooth  = Lag.kr( compound, 10 )
////           smooth.poll( 2 )
//            1.react( smooth ) { data =>
//               val Seq( loud, centr, flat ) = data
//               ProcTxn.atomic { implicit tx =>
//                  pid.
//               }
//            }
//            0
//         }
//      }
//   }

   def apply( id: Int, transit: Boolean )( implicit tx: ProcTxn ) : Plate = {
      val pColl   = filter( (id + 1).toString + "+" ) { graph { in => in }} make
      val pDummy  = factory( "@" ).make
      pDummy ~> pColl
      var plate: Plate = null
      val pAna    = (diff( "ana" + (id+1) ) {
//         val pid = pScalar( "id", ParamSpec( 0, NUM_PLATES - 1, step = 1 ), 0 )
         val ploud   = pScalar( "loud", ParamSpec( 0, 12 ), 0 )  // for display only
         val pcentr  = pScalar( "centr", ParamSpec( 20, 2000, ExpWarp ), 20 )  // for display only
         val pflat   = pScalar( "flat", ParamSpec( 0, 1 ), 0 )  // for display only

         graph { in =>
            val bufID   = bufEmpty( 1024 ).id
            val chain   = FFT( bufID, Mix( in ))
            val loud    = Loudness.kr( chain )
//            val centr   = SpecCentroid.kr( chain )
            val centr   = SpecPcile.kr( chain )
            val flat0    = SpecFlatness.kr( chain )
            val flatc   = CheckBadValues.kr( flat0, post = 0 )
            val flat    = Gate.kr( flat0, flatc === 0 )
//          List( loud, centr, flat ).poll( 2 )
            val compound= List( loud, centr, flat )
            val smooth  = Lag.kr( compound, 10 )
//           smooth.poll( 2 )
            1.react( smooth ) { data =>
               val Seq( loud, centr, flat ) = data
               ProcTxn.spawnAtomic { implicit tx =>
                  plate.newAnalysis( loud, centr, flat )
                  // display
                  val me = plate.analyzer
                  me.control( "loud" ).v    = loud
                  me.control( "centr" ).v   = centr
                  me.control( "flat" ).v    = flat
               }
            }
            0
         }
      }).make
      plate = new Plate( id, pColl, pAna )
      pColl ~> pAna
      pColl ~> SemiNuages.collMaster.audioInput( "in" + (id+1) )
      pAna.play
      pDummy.dispose
      plate
   }

   private val stopAndDisposeListener = new Proc.Listener {
      def updated( u: Proc.Update ) {
         if( !u.state.fading && (!u.state.playing || u.state.bypassed) ) {
            // XXX workaround: CCSTM still has the previous txn visible,
            // hence we need to wait a bit longer :-(
//EventQueue.invokeLater { new Runnable { def run {
            if( verbose ) println( "" + new java.util.Date() + " FINAL-DISPOSE " + u.proc )
            disposeProc( u.proc ) // ProcTxn.atomic { implicit tx => }
//}}}
         }
      }
   }

   private def disposeProc( proc: Proc ) {
      ProcTxn.atomic { implicit t =>
         proc.anatomy match {
            case ProcFilter => disposeFilter( proc )
            case _ => disposeGenDiff( proc )
         }
      }
   }

   // XXX copied from Nuages. we should have this going into SoundProcesses directly somehow
   private def disposeFilter( proc: Proc )( implicit tx: ProcTxn ) {
      val in   = proc.audioInput( "in" )
      val out  = proc.audioOutput( "out" )
      val ines = in.edges.toSeq
      val outes= out.edges.toSeq
      if( ines.size > 1 ) println( "WARNING : Filter is connected to more than one input!" )
      if( verbose && outes.nonEmpty ) println( "" + new java.util.Date() + " " + out + " ~/> " + outes.map( _.in ))
      outes.foreach( oute => {
//         if( verbose ) println( "" + out + " ~/> " + oute.in )
         out ~/> oute.in
      })
      ines.headOption.foreach( ine => {
         if( verbose ) println( "" + new java.util.Date() + " " + ine.out + " ~> " + outes.map( _.in ))
         outes.foreach( oute => {
//            if( verbose ) println( "" + ine.out + " ~> " + oute.in )
            ine.out ~> oute.in
         })
      })
      // XXX tricky: this needs to be last, so that
      // the pred out's bus isn't set to physical out
      // (which is currently not undone by AudioBusImpl)
      if( verbose && ines.nonEmpty ) println( "" + new java.util.Date() + " " + ines.map( _.out ) + " ~/> " + in )
      ines.foreach( ine => {
         ine.out ~/> in
      })
      proc.dispose
   }

   // XXX copied from Nuages. we should have this going into SoundProcesses directly somehow
   private def disposeGenDiff( proc: Proc )( implicit tx: ProcTxn ) {
//      val toDispose = MSet.empty[ Proc ]
//      addToDisposal( toDispose, proc )
//      toDispose.foreach( p => {
val p = proc
         val ines = p.audioInputs.flatMap( _.edges ).toSeq // XXX
         val outes= p.audioOutputs.flatMap( _.edges ).toSeq // XXX
         outes.foreach( oute => oute.out ~/> oute.in )
         ines.foreach( ine => ine.out ~/> ine.in )
         p.dispose
//      })
   }

   private def stopAndDispose( rp: RunningProc )( implicit tx: ProcTxn ) {
      val p     = rp.proc
      val state = p.state
//println( "STOP-AND-DISPOSE " + p + " -> " + state + " / " + tx.transit )
      if( !state.fading && (!state.playing || state.bypassed || (tx.transit == Instant)) ) {
//println( ".......INSTANT" )
         p.dispose
      } else {
         p.addListener( stopAndDisposeListener )
         p.anatomy match {
            case ProcFilter => {
//println( ".......BYPASS" )
               p.bypass
            }
            case _ => {
//println( ".......STOP " + (new java.util.Date()) )
               p.stop
            }
         }
      }
   }

   case class RunningProc( proc: Proc, context: SoundContext, startTime: Long, deathTime: Long )
}

class Plate( val id: Int, val collector: Proc, val analyzer: Proc ) {
   import Plate._
   
   private val loudnessRef = Ref( 0.0 )
   private val centroidRef = Ref( 0.0 )
   private val flatnessRef = Ref( 0.0 )

   private val silenceCount   = Ref( 0)
   private val induction      = Ref( Set.empty[ RunningProc ])

   def loudness( implicit tx: ProcTxn ) = loudnessRef()
   def centroid( implicit tx: ProcTxn ) = centroidRef()
   def flatness( implicit tx: ProcTxn ) = flatnessRef()

   override def toString = "plate<" + id + ">"

   def newAnalysis( loud: Double, centr: Double, flat: Double )( implicit tx: ProcTxn ) {
      loudnessRef.set( loud )
      centroidRef.set( centr )
      flatnessRef.set( flat )

      // ---- act ----
      if( loud < MIN_LOUDNESS_THRESH ) {
         val cnt = silenceCount() + 1
         silenceCount.set( cnt )
         if( cnt >= MIN_LOUDNESS_COUNT ) {
//            println( this.toString + " : make some noise" )
            makeSomeNoise
         }
      } else {
         silenceCount.set( 0 )
      }
   }

   private def makeSomeNoise( implicit tx: ProcTxn ) {
      induction.transform( _ + createProc( Material.all ))
   }

//   toStop foreach { rp => xfade( exprand( rp.context.minFade, rp.context.maxFade )) {
//      if( verbose ) println( "" + new java.util.Date() + " STOPPING OBSOLETE " + rp )
//      stopAndDispose( rp )
////         rp.proc.dispose // stop
//   }}

   private def insertAndPlayGen( rp: RunningProc, fdt: Double )( implicit tx: ProcTxn ) {
      rp.proc ~> collector // CupolaNuages.fieldCollectors( rp.context.field )
      xfade( fdt ) { rp.proc.play }
   }

   private def createProc( candidates: Seq[ SoundContext ])( implicit tx: ProcTxn ) : RunningProc = {
      val now     = System.currentTimeMillis
      val weightSum  = candidates.foldLeft( 0.0 ) { (sum, c) => sum + c.weight }
      val c       = wchoose( candidates ) { _.weight / weightSum }
      val f       = c.settings.createProcFactory( c.name )
      val death   = (exprand( c.minDur, c.maxDur ) * 1000).toLong + now
      val proc    = f.make
      c.settings.prepareForPlay( proc )
      val rp      = RunningProc( proc, c, now, death )
      if( verbose ) println( "" + new java.util.Date() + " STARTING (SPARSE) " + rp )
      insertAndPlayGen( rp, exprand( rp.context.minFade, rp.context.maxFade ))
      rp
   }

//   private def makeTapePlayer( name: String, file: String )( implicit tx: ProcTxn ) = {
//      ProcDemiurg.factories.find( _.name == name ) getOrElse gen( name ) {
//         val pspeed  = pControl( "speed", ParamSpec( 0.1f, 10, ExpWarp ), speed )
//         val pamp    = pControl( "amp",   ParamSpec( 0.1f, 10, ExpWarp ), gain.dbamp )
//         val ppos    = pScalar(  "pos",   ParamSpec( 0, 1 ), 0 )
//         graph {
//            val fullPath   = Dissemination.BASE_PATH + File.separator + "audio_work" + File.separator + file
//            val afSpec     = audioFileSpec( fullPath )
//            val startPos   = ppos.v
//            val startFrame = (startPos * afSpec.numFrames).toLong
//            val buf        = bufCue( fullPath, startFrame )
//            val bufID      = buf.id
//            val speed      = pspeed.kr * BufRateScale.ir( bufID )
//            val d          = VDiskIn.ar( afSpec.numChannels, bufID, speed, loop = 1 )
////               val frame   = d.reply
////               (frame.carry( pspeed.v * b.sampleRate ) / b.numFrames) ~> ppos
//            val liveFrame  = Integrator.ar( K2A.ar( speed ))
//            val livePos    = ((liveFrame / BufFrames.ir( bufID )) + startPos) % 1.0f
////               livePos ~> ppos
//            d * pamp.kr
//         }
//      }
//   }
}