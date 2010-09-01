/*
 *  Plate.scala
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

import de.sciss.synth._
import collection.immutable.{ IndexedSeq => IIdxSeq }
import proc._
import DSL._
import ugen._
import java.io.File
import Util._
import Dissemination._

object Plate {
   val verbose = true
   
   val MIN_LOUDNESS_THRESH = 1.0
   val MIN_LOUDNESS_COUNT  = 10     


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
//println( "AQUI " + id )
               ProcTxn.spawnAtomic { implicit tx =>
//java.awt.EventQueue.invokeLater( new Runnable { def run = ProcTxn.atomic { implicit tx =>
                  plate.newAnalysis( loud, centr, flat )
                  // display
                  val me = plate.analyzer
                  me.control( "loud" ).v    = loud
                  me.control( "centr" ).v   = centr
                  me.control( "flat" ).v    = flat
               }
//            })
            }
            0
         }
      }).make

      val pRec = (diff( "rec" + id ) {
         val pdur = pScalar( "dur", ParamSpec( 1, 120 ), 1 ) 
         graph { in =>
            val b = bufRecord( RECORD_PATH + fs + "plate" + id + ".aif",
               in.numOutputs )
            DiskOut.ar( b.id, in )
            val done = Done.kr( Line.kr( dur = pdur.ir ))
            done.react {
               ProcTxn.spawnAtomic { implicit tx => plate.recordDone }
            }
//            FreeSelf.kr( done )
            PauseSelf.kr( done ) // free-self not yet recognized by proc, so we avoid a /n_free node not found
         }
      }).make

      plate = new Plate( id, pColl, pAna, pRec )
      pColl ~> pAna
      pColl ~> SemiNuages.collMaster.audioInput( "in" + (id+1) )
      pColl ~> pRec
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

class Plate( val id: Int, val collector: Proc, val analyzer: Proc, val recorder: Proc ) {
   import Plate._
   
   private val loudnessRef = Ref( 0.0 )
   private val centroidRef = Ref( 0.0 )
   private val flatnessRef = Ref( 0.0 )

   private val silenceCount   = Ref( 0)
   private val induction      = Ref( Set.empty[ RunningProc ])

   def loudness( implicit tx: ProcTxn ) = loudnessRef()
   def centroid( implicit tx: ProcTxn ) = centroidRef()
   def flatness( implicit tx: ProcTxn ) = flatnessRef()

   private val energyCons  = Ref( 0.0 )
   private val energyProd  = Ref( 0.0 )
// private val energyBal   = Ref( 0.0 )  // always equal to prod minus consume
   private val exhausted   = Ref( 0 )

   override def toString = "plate<" + id + ">"

   private var neighbour1: Option[ Plate ] = None
   private var neighbour2: Option[ Plate ] = None
   private var neighbourW: Int = 0

   private val recording = Ref( false )

   def initNeighbours( n1: Option[ Plate ], n2: Option[ Plate ]) {
      neighbour1  = n1
      neighbour2  = n2
      neighbourW  = (n1 :: n2 :: Nil).count( _.isEmpty ) + 1
   }

   def newAnalysis( loud0: Double, centr0: Double, flat0: Double )( implicit tx: ProcTxn ) {
      // eventually we might tune in some kind of correction curves
      val loud    = (loud0 - 0.192486).max( 0.0 )
      val centr   = centr0
      val flat    = flat0

      loudnessRef.set( loud )
      centroidRef.set( centr )
      flatnessRef.set( flat )
      val exhaust = exhausted()

      // ---- minimum loudness ----
      if( loud < MIN_LOUDNESS_THRESH ) {
         val cnt = silenceCount() + 1
         silenceCount.set( cnt )
         if( (cnt >= MIN_LOUDNESS_COUNT) && (exhaust == 0) ) {
//            println( this.toString + " : make some noise" )
            consumeSomeNoise
         }
      } else {
         silenceCount.set( 0 )
      }

      val eCons = energyCons() + (1.0 - flat) * loud
      val eProd = energyProd() +
         neighbour1.map( n => n.flatness * n.loudness * neighbourW ).getOrElse( 0.0 ) +
         neighbour2.map( n => n.flatness * n.loudness * neighbourW ).getOrElse( 0.0 )
      val eBal  = eProd - eCons
      energyCons.set( eCons )
      energyProd.set( eProd )

      if( exhaust > 0 ) {
         exhausted.set( exhaust - 1 )
      }

//      if( verbose ) println( this.toString + " : energy balance = " + eBal )
      if( eBal < -100 ) {
         produceSomeNoise
      }
   }

   private def consumeSomeNoise( implicit tx: ProcTxn ) {
      induction.transform( _ + createProc( Material.all ))
   }

   private def produceSomeNoise( implicit tx: ProcTxn ) {
      if( recording.swap( true )) return

//      val pRec = recFactory.make
      recorder.control( "dur" ).v = nextPowerOfTwo( (exprand( 8, 90 ) * SAMPLE_RATE).toInt ) / SAMPLE_RATE
//      collector ~> recorder
      recorder.play
//      recordProc.set( Some( pRec ))
   }

   def recordDone( implicit tx: ProcTxn ) {
      println( this.toString + " RECORD DONE" )
//      recordProc.swap( None ).foreach( _.dispose )
      recorder.stop

      val tmpA    = WORK_PATH   + fs + "tmp-a" + id + ".aif"
      val tmpB    = WORK_PATH   + fs + "tmp-b" + id + ".aif"
      val recPath = RECORD_PATH + fs + "plate" + id + ".aif"
      val docRvs  = FScape.UnaryOp(
         in       = recPath,
         out      = tmpA,
         reverse  = true
      )
      val docMirr = FScape.BinaryOp(
         in1      = recPath,
         in2      = tmpA,
         offset2  = "-1.0",
         length2  = "2.0",
         out      = tmpB
      )
      val docFFT  = FScape.Fourier(
         in       = tmpB,
         out      = tmpA,
         gain     = FScape.Gain.normalized
      )
      val docFrz  = FScape.Kriechstrom(
         in       = tmpA,
         out      = tmpB,
         length   = "1.5"
      )
      val docIFFT = FScape.Fourier(
         in       = tmpB,
         out      = tmpA,
         gain     = FScape.Gain.normalized,
         inverse  = true
      )
      val docHalf = FScape.UnaryOp(
         in       = tmpA,
         out      = tmpB,
         length   = "0.5"
      )
      FScape.processChain( "PlateFrz", docRvs :: docMirr :: docFFT :: docFrz :: docIFFT :: docHalf :: Nil ) { success =>
         if( success ) {
            println( "Done all!" )
         } else {
//            recording.set( false )
         }
      }

//energyCons.set( 0.0 )   // XXX just for now
//energyProd.set( 0.0 )   // so we don't start recording again straight away
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