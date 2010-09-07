/*
 *  SemiNuages.scala
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

import de.sciss.synth.{ EnvSeg => S, _ }
import de.sciss.synth.proc._
import de.sciss.nuages.NuagesFrame
import collection.breakOut
import collection.immutable.{ IndexedSeq => IIdxSeq }
import ugen._
import scala.Float.{ PositiveInfinity => inf }
import java.io.File

/**
 *    @version 0.12, 01-Aug-10
 */
object SemiNuages extends {
   import DSL._
   import Dissemination._

   val NUM_LOOPS        = 7
   val LOOP_DUR         = 30

   var f : NuagesFrame = _
   var masterSynth : Synth  = _

//   var plateCollectors = IIdxSeq[ Proc ]() // = _
   var plates: IIdxSeq[ Plate ] = _
   var collMaster: Proc = _
   var pMaster:    Proc = _

   def init( s: Server, f: NuagesFrame ) = ProcTxn.spawnAtomic { implicit tx =>

      // -------------- DIFFUSIONS --------------

//      val goAll = diff( "O-all" ) {
//         val pamp  = pAudio( "amp", ParamSpec( 0.01, 10, ExpWarp ), 1 )
//         val pout  = pAudioOut( "out", config.masterBus.map( RichBus.wrap( _ )))
//
//         graph { in =>
//            val sig          = (in * Lag.ar( pamp.ar, 0.1 )).outputs
//            val inChannels   = sig.size
//            val outChannels  = MASTER_NUMCHANNELS
//            val sig1         = List.tabulate( outChannels )( ch => sig( ch % inChannels ))
//            pout.ar( sig1 )
//         }
//      }

//      diff( "O-pan" ) {
//         val pspread = pControl( "spr",  ParamSpec( 0.0, 1.0 ), 0.25 ) // XXX rand
//         val prota   = pControl( "rota", ParamSpec( 0.0, 1.0 ), 0.0 )
//         val pbase   = pControl( "azi",  ParamSpec( 0.0, 360.0 ), 0.0 )
//         val pamp    = pAudio( "amp", ParamSpec( 0.01, 10, ExpWarp ), 1 )
//         val pout    = pAudioOut( "out", config.masterBus.map( RichBus.wrap( _ )))
//
//         graph { in =>
//            val baseAzi       = Lag.kr( pbase.kr, 0.5 ) + IRand( 0, 360 )
//            val rotaAmt       = Lag.kr( prota.kr, 0.1 )
//            val spread        = Lag.kr( pspread.kr, 0.5 )
//            val inChannels   = in.numOutputs
//            val outChannels  = MASTER_NUMCHANNELS
////            val sig1         = List.tabulate( outChannels )( ch => sig( ch % inChannels ))
//            val rotaSpeed     = 0.1
//            val inSig         = (in * Lag.ar( pamp.ar, 0.1 )).outputs
//            val noise         = LFDNoise1.kr( rotaSpeed ) * rotaAmt * 2
//            val outSig: Array[ GE ] = Array.fill( outChannels )( 0 )
//            val altern        = false
//            for( inCh <- 0 until inChannels ) {
//               val pos0 = if( altern ) {
//                  (baseAzi / 180) + (inCh / outChannels * 2);
//               } else {
//                  (baseAzi / 180) + (inCh / inChannels * 2);
//               }
//               val pos = pos0 + noise
//
//             // + rota
////				   w	 = inCh / (inChannels -1);
////				   level = ((1 - levelMod) * w) + (1 - w);
//               val level   = 1   // (1 - w);
//               val width   = (spread * (outChannels - 2)) + 2
//               val pan     = PanAz.ar( outChannels, inSig( inCh ), pos, level, width, 0 )
//               pan.outputs.zipWithIndex.foreach( tup => {
//                  val (chanSig, i) = tup
//                  outSig( i ) = outSig( i ) + chanSig
//               })
//            }
//            pout.ar( outSig.toSeq )
//         }
//      }

//      diff( "O-rnd" ) {
//         val pamp  = pAudio( "amp", ParamSpec( 0.01, 10, ExpWarp ), 1 )
//         val pfreq = pControl( "freq", ParamSpec( 0.01, 10, ExpWarp ), 0.1 )
//         val ppow  = pControl( "pow", ParamSpec( 1, 10 ), 2 )
//         val plag  = pControl( "lag", ParamSpec( 0.1, 10 ), 1 )
//         val pout  = pAudioOut( "out", config.masterBus.map( RichBus.wrap( _ )))
//
//         graph { in =>
//            val sig          = (in * Lag.ar( pamp.ar, 0.1 )).outputs
//            val inChannels   = sig.size
//            val outChannels  = MASTER_NUMCHANNELS
//            val sig1: GE     = List.tabulate( outChannels )( ch => sig( ch % inChannels ))
//            val freq         = pfreq.kr
//            val lag          = plag.kr
//            val pw           = ppow.kr
//            val rands        = Lag.ar( TRand.ar( 0, 1, Dust.ar( List.fill( outChannels )( freq ))).pow( pw ), lag )
//            val outSig       = sig1 * rands
//            pout.ar( outSig )
//         }
//      }

      // -------------- GENERATORS --------------

      gen( "test" ) {
         val pf1  = pControl( "f1", ParamSpec( 0.1, 10, ExpWarp ), 1 )
         val pf2  = pControl( "f2", ParamSpec( 50, 15000, ExpWarp ), 440 )
         val pphas= pScalar( "phas", ParamSpec( 0, 1 ), 0 )
         val pq   = pControl( "q", ParamSpec( 1, 100, ExpWarp ), 20 )
         graph {
//            val env = Decay.ar( Impulse.ar( pf1.kr ), 1 )
            val freq = pf1.kr
            val env = Decay.ar( LFPulse.ar( freq, pphas.ir, freq / SampleRate.ir ), 1 )
            Resonz.ar( WhiteNoise.ar( 1 ) * env, pf2.kr, pq.kr.reciprocal )
         }
      }

//      // NuagesUMic
//      val loopFrames = (LOOP_DUR * s.sampleRate).toInt
//      val loopBufs   = Array.fill[ Buffer ]( NUM_LOOPS )( Buffer.alloc( s, loopFrames, 2 ))
//      val loopBufIDs: Seq[ Int ] = loopBufs.map( _.id )( breakOut )
//
//      gen( "loop" ) {
//         val pbuf    = pControl( "buf",   ParamSpec( 0, NUM_LOOPS - 1, LinWarp, 1 ), 0 )
//         val pspeed  = pControl( "speed", ParamSpec( 0.125, 2.3511, ExpWarp ), 1 )
//         val pstart  = pControl( "start", ParamSpec( 0, 1 ), 0 )
//         val pdur    = pControl( "dur",   ParamSpec( 0, 1 ), 1 )
//         graph {
//            val trig1	   = LocalIn.kr( 1 )
//            val gateTrig1	= PulseDivider.kr( trig = trig1, div = 2, start = 1 )
//            val gateTrig2	= PulseDivider.kr( trig = trig1, div = 2, start = 0 )
//            val startFrame = pstart.kr * loopFrames
//            val numFrames  = pdur.kr * (loopFrames - startFrame)
//            val lOffset	   = Latch.kr( in = startFrame, trig = trig1 )
//            val lLength	   = Latch.kr( in = numFrames,  trig = trig1 )
//            val speed      = pspeed.kr
//            val duration	= lLength / (speed * SampleRate.ir) - 2
//            val gate1	   = Trig1.kr( in = gateTrig1, dur = duration )
//            val env		   = Env.asr( 2, 1, 2, linShape )	// \sin
//            val bufID      = Select.kr( pbuf.kr, loopBufIDs )
//            val play1	   = PlayBuf.ar( 2, bufID, speed, gateTrig1, lOffset, loop = 0 )
//            val play2	   = PlayBuf.ar( 2, bufID, speed, gateTrig2, lOffset, loop = 0 )
//            val amp0		   = EnvGen.kr( env, gate1 )  // 0.999 = bug fix !!!
//            val amp2		   = 1.0 - amp0.squared
//            val amp1		   = 1.0 - (1.0 - amp0).squared
//            val sig     	= (play1 * amp1) + (play2 * amp2)
//            LocalOut.kr( Impulse.kr( 1.0 / duration.max( 0.1 )))
//            sig
//         }
//      }

//      gen( "mic" ) {
//         val pboost  = pAudio( "gain", ParamSpec( 0.1, 10, ExpWarp ), 0.1 /* 1 */)
//         val pfeed   = pAudio( "feed", ParamSpec( 0, 1 ), 0 )
//         graph {
//            val off        = if( INTERNAL_AUDIO ) 0 else MIC_OFFSET
//            val boost      = pboost.ar
//            val pureIn	   = In.ar( NumOutputBuses.ir + off, 2 ) * boost
//            val bandFreqs	= List( 150, 800, 3000 )
//            val ins		   = HPZ1.ar( pureIn ) // .outputs
//            var outs: GE   = 0
//            var flt: GE    = ins
//            bandFreqs.foreach( maxFreq => {
//               val band = if( maxFreq != bandFreqs.last ) {
//                  val res  = LPF.ar( flt, maxFreq )
//                  flt	   = HPF.ar( flt, maxFreq )
//                  res
//               } else {
//                  flt
//               }
//               val amp		= Amplitude.kr( band, 2, 2 )
//               val slope	= Slope.kr( amp )
//               val comp		= Compander.ar( band, band, 0.1, 1, slope.max( 1 ).reciprocal, 0.01, 0.01 )
//               outs		   = outs + comp
//            })
//            val dly        = DelayC.ar( outs, 0.0125, LFDNoise1.kr( 5 ).madd( 0.006, 0.00625 ))
//            val feed       = pfeed.ar * 2 - 1
//            XFade2.ar( pureIn, dly, feed )
//         }
//      }

//      gen( "sum_rec" ) {
//         val pbuf    = pControl( "buf",  ParamSpec( 0, NUM_LOOPS - 1, LinWarp, 1 ), 0 )
//         val pfeed   = pControl( "feed", ParamSpec( 0, 1 ), 0 )
//         val ploop   = pControl( "loop", ParamSpec( 0, 1, LinWarp, 1 ), 0 )
//         graph {
//            val in      = InFeedback.ar( masterBus.index, masterBus.numChannels )
//            val w       = 2.0 / in.numOutputs
//            var sig     = Array[ GE ]( 0, 0 )
//            in.outputs.zipWithIndex.foreach( tup => {
//               val (add, ch) = tup
//               sig( ch % 2 ) += add
//            })
//            val sig1    = sig.toSeq * w
//            val bufID   = Select.kr( pbuf.kr, loopBufIDs )
//            val feed    = pfeed.kr
//            val prelvl  = feed.sqrt
//            val reclvl  = (1 - feed).sqrt
//            val loop    = ploop.kr
//            val rec     = RecordBuf.ar( sig1, bufID, recLevel = reclvl, preLevel = prelvl, loop = loop )
//            Silent.ar( 2 )// dummy thru
//         }
//      }

      // -------------- FILTERS --------------

      def mix( in: GE, flt: GE, mix: ProcParamAudio ) : GE = LinXFade2.ar( in, flt, mix.ar * 2 - 1 )
      def pMix = pAudio( "mix", ParamSpec( 0, 1 ), 1 )

      // NuagesUHilbert
      // NuagesUMagBelow

//      filter( "rec" ) {
//         val pbuf    = pControl( "buf",  ParamSpec( 0, NUM_LOOPS - 1, LinWarp, 1 ), 0 )
//         val pfeed   = pControl( "feed", ParamSpec( 0, 1 ), 0 )
//         val ploop   = pControl( "loop", ParamSpec( 0, 1, LinWarp, 1 ), 0 )
//         graph { in =>
//            val bufID   = Select.kr( pbuf.kr, loopBufIDs )
//            val feed    = pfeed.kr
//            val prelvl  = feed.sqrt
//            val reclvl  = (1 - feed).sqrt
//            val loop    = ploop.kr
//            val rec     = RecordBuf.ar( in, bufID, recLevel = reclvl, preLevel = prelvl, loop = loop )
//            in  // dummy thru
//         }
//      }

      filter( "achil") {
         val pspeed  = pAudio( "speed", ParamSpec( 0.125, 2.3511, ExpWarp ), 0.5 )
         val pmix    = pMix

         graph { in =>
            val speed	   = Lag.ar( pspeed.ar, 0.1 )
            val numFrames  = sampleRate.toInt
            val numChannels= in.numOutputs
            val buf        = bufEmpty( numFrames, numChannels )
            val bufID      = buf.id
            val writeRate  = BufRateScale.kr( bufID )
            val readRate   = writeRate * speed
            val readPhasor = Phasor.ar( 0, readRate, 0, numFrames )
            val read			= BufRd.ar( numChannels, bufID, readPhasor, 0, 4 )
            val writePhasor= Phasor.ar( 0, writeRate, 0, numFrames )
            val old			= BufRd.ar( numChannels, bufID, writePhasor, 0, 1 )
            val wet0 		= SinOsc.ar( 0, ((readPhasor - writePhasor).abs / numFrames * math.Pi) )
            val dry			= 1 - wet0.squared
            val wet			= 1 - (1 - wet0).squared
            BufWr.ar( (old * dry) + (in * wet), bufID, writePhasor )
            mix( in, read, pmix )
         }
      }

//      filter( "a-gate" ) {
//         val pamt = pAudio( "amt", ParamSpec( 0, 1 ), 1 )
//         val pmix = pMix
//         graph { in =>
//            val amount = Lag.ar( pamt.ar, 0.1 )
//            val flt = Compander.ar( in, in, Amplitude.ar( in * (1 - amount ) * 5 ), 20, 1, 0.01, 0.001 )
//            mix( in, flt, pmix )
//         }
//      }


      filter( "a-hilb" ) {
         val pmix = pMix
         graph { in =>
            val flt = in.outputs map { ch =>
               val hlb  = Hilbert.ar( DelayN.ar( ch, 0.01, 0.01 ))
               val hlb2 = Hilbert.ar( Normalizer.ar( ch, dur = 0.02 ))
               (hlb \ 0) * (hlb2 \ 0) - (hlb \ 1 * hlb2 \ 1)
            }
            mix( in, flt, pmix )
         }
      }

      // ---- DISSEM FILTERS ----

//      Seq( "m", "s" ).foreach( suff => filter( "fl_ahilb" + suff ) {
////         val pmix = pMix
//         graph { in =>
//            var flt: GE = Seq.fill( in.numOutputs )( 0.0 )
//            in.outputs foreach { ch =>
//               val Seq( hlbRe1, hlbIm1 )  = Hilbert.ar( DelayN.ar( ch, 0.01, 0.01 )).outputs
//               val Seq( hlbRe2, hlbIm2 )  = Hilbert.ar( Normalizer.ar( ch, dur = 0.02 )).outputs
//               flt += hlbRe1 * hlbRe2 - hlbIm1 * hlbIm2
//            }
////            mix( in, flt, pmix )
//            val freq: GE = if( suff == "m" ) {
//               1.0/20
//            } else {
//               Seq( 1.0/20, 1.0/20 )
//            }
//            val dust       = Dust2.kr( freq )
//            val mix        = Lag.kr( Latch.kr( dust, dust ), 20 )
//            LinXFade2.ar( in, flt, mix )
//         }
//      })

//      filter( "fl_verb" ) {
//         val pextent = pScalar( "size", ParamSpec( 0, 1 ), 0.5 )
////         val pcolor  = pControl( "color", ParamSpec( 0, 1 ), 0.5 )
//         val pmix    = pMix
//         graph { in =>
//            val extent     = pextent.ir
////            val color	   = Lag.kr( pcolor.kr, 0.1 )
//            val freq       = 1.0/20
//            val dust       = Dust.kr( freq )
//            val color      = Lag.kr( Latch.kr( dust, dust ), 20 )
//
//            val i_roomSize	= LinExp.ir( extent, 0, 1, 1, 100 )
//            val i_revTime  = LinExp.ir( extent, 0, 1, 0.3, 20 )
//            val spread	   = 15
//            val numChannels= in.numOutputs
//            val ins        = in.outputs
//            val verbs      = (ins :+ ins.last).grouped( 2 ).toSeq.flatMap( pair =>
//               (GVerb.ar( Mix( pair ), i_roomSize, i_revTime, color, color, spread, 0, 1, 0.7, i_roomSize ) * 0.3).outputs
//            )
//// !! BUG IN SCALA 2.8.0 : CLASSCASTEXCEPTION
//// weird stuff goin on with UGenIn seqs...
//            val flt: GE     = Vector( verbs.take( numChannels ): _* ) // drops last one if necessary
//            mix( in, flt, pmix )
//         }
//      }

//      Seq( "m", "s" ).foreach( suff => filter( "fl_above" + suff ) {
////         val pthresh = pControl( "thresh", ParamSpec( 1.0e-3, 1.0e-1, ExpWarp ), 1.0e-2 )
//         val pbright = pControl( "bright", ParamSpec( 0, 1 ), 0.5 )
//         val pmix = pMix
//         graph { in =>
//            val numChannels   = in.numOutputs
////            val thresh		   = pthresh.kr
//            val freq: GE = if( suff == "m" ) {
//               1.0/20
//            } else {
//               Seq.fill( 2 )( 1.0/20 )
//            }
//            val thresh        = LFDNoise1.kr( freq ).linexp( -1, 1, 1.0e-3, 1.0e-1 )
//            val env			   = Env( 0.0, List( S( 0.2, 0.0, stepShape ), S( 0.2, 1.0, linShape )))
//            val ramp			   = EnvGen.kr( env )
//            val volume		   = LinLin.kr( thresh, 1.0e-3, 1.0e-1, 32, 4 )
//            val bufIDs        = Seq.fill( numChannels )( bufEmpty( 1024 ).id )
//            val bright        = pbright.kr * 2 - 1
//            val chain1 		   = FFT( bufIDs, LinXFade2.ar( in * 0.5, HPZ1.ar( in ), bright ))
//            val chain2        = PV_MagAbove( chain1, thresh )
//            val sig0          = volume * IFFT( chain2 )
//            val flt			   = LinXFade2.ar( sig0 * 0.5, LPZ1.ar( sig0 ), bright ) * ramp
//
//            // account for initial dly
//            val env2          = Env( 0.0, List( S( BufDur.kr( bufIDs ) * 2, 0.0, stepShape ), S( 0.2, 1, linShape )))
//            val wet			   = EnvGen.kr( env2 )
//            val sig			   = (in * (1 - wet).sqrt) + (flt * wet)
//            mix( in, sig, pmix )
//         }
//      })

//      Seq( "m", "s" ).foreach( suff => filter( "fl_filt" + suff ) {
////         val pfreq   = pAudio( "freq", ParamSpec( -1, 1 ), 0.54 )
//         val pmix    = pMix
//         graph { in =>
//            def envGen: (GE, GE) = {
//               val offPeriod  = Dwhite( 0, 1, 1 ).linexp( 0, 1, 8, 16 )
//               val onPeriod   = Dwhite( 0, 1, 1 ).linexp( 0, 1, 4, 8 )
//               val fadePeriod = Dwhite( 4, 16, 1 )
//               val periods    = Dseq( Seq( offPeriod, fadePeriod, onPeriod, fadePeriod ), inf )
//               val lowFreqs   = Dwhite( -0.5, -0.3, 1 )
//               val highFreqs  = Dwhite( 0.3, 0.5, 1 )
//               val loHiFreqs  = Drand( Seq( lowFreqs, highFreqs ))
//               val freqs      = Dseq( Dstutter( 2, Dseq( Seq[ GE ]( 0, loHiFreqs ))), inf )
//               (freqs, periods)
//            }
//            val (freqs, periods) = if( suff == "m" ) {
//               envGen
//            } else {
//               val (freqsL, periodsL) = envGen
//               val (freqsR, periodsR) = envGen
//               val freqs: GE = Seq( freqsL, freqsR )
//               val periods: GE = Seq( periodsL, periodsR )
//               (freqs, periods)
//            }
//            val normFreq	= DemandEnvGen.ar( freqs, periods )
////            normFreq.poll( Impulse.kr( 1 ), "n" )
//            val lowFreqN	= Lag.ar( Clip.ar( normFreq, -1, 0 ))
//            val highFreqN  = Lag.ar( Clip.ar( normFreq,  0, 1 ))
//            val lowFreq		= lowFreqN.linexp( -1, 0, 30, 20000 )
//            val highFreq	= highFreqN.linexp( 0, 1, 30, 20000 )
//            val lowMix		= Clip.ar( lowFreqN * -10.0, 0, 1 )
//            val highMix		= Clip.ar( highFreqN * 10.0, 0, 1 )
//            val dryMix		= 1 - (lowMix + highMix)
//            val lpf			= LPF.ar( in, lowFreq ) * lowMix
//            val hpf			= HPF.ar( in, highFreq ) * highMix
//            val dry			= in * dryMix
//            val flt			= dry + lpf + hpf
//            mix( in, flt, pmix )
//         }
//      })

//      filter( "filt" ) {
//         val pfreq   = pAudio( "freq", ParamSpec( -1, 1 ), 0.54 )
//         val pmix    = pMix
//
//         graph { in =>
//            val normFreq	= pfreq.ar
//            val lowFreqN	= Lag.ar( Clip.ar( normFreq, -1, 0 ))
//            val highFreqN	= Lag.ar( Clip.ar( normFreq,  0, 1 ))
//            val lowFreq		= LinExp.ar( lowFreqN, -1, 0, 30, 20000 )
//            val highFreq	= LinExp.ar( highFreqN, 0, 1, 30, 20000 )
//            val lowMix		= Clip.ar( lowFreqN * -10.0, 0, 1 )
//            val highMix		= Clip.ar( highFreqN * 10.0, 0, 1 )
//            val dryMix		= 1 - (lowMix + highMix)
//            val lpf			= LPF.ar( in, lowFreq ) * lowMix
//            val hpf			= HPF.ar( in, highFreq ) * highMix
//            val dry			= in * dryMix
//            val flt			= dry + lpf + hpf
//            mix( in, flt, pmix )
//         }
//      }

//      filter( "frgmnt" ) {
//   		val pspeed  = pAudio(   "speed", ParamSpec( 0.125, 2.3511, ExpWarp ), 1 )
//		   val pgrain  = pControl( "grain", ParamSpec( 0, 1 ), 0.5 )
//		   val pfeed   = pAudio(   "fb",    ParamSpec( 0, 1 ), 0 )
//         val pmix    = pMix
//
//         graph { in =>
//            val bufDur        = 4.0
//            val numFrames     = (bufDur * sampleRate).toInt
//            val numChannels   = in.numOutputs
//            val buf           = bufEmpty( numFrames, numChannels )
//            val bufID         = buf.id
//
//            val feedBack	   = Lag.ar( pfeed.ar, 0.1 )
//            val grain	      = pgrain.kr // Lag.kr( grainAttr.kr, 0.1 )
//            val maxDur	      = LinExp.kr( grain, 0, 0.5, 0.01, 1.0 )
//            val minDur	      = LinExp.kr( grain, 0.5, 1, 0.01, 1.0 )
//            val fade		      = LinExp.kr( grain, 0, 1, 0.25, 4 )
//            val rec		      = (1 - feedBack).sqrt
//            val pre		      = feedBack.sqrt
//            val trig		      = LocalIn.kr( 1 )
//            val white	      = TRand.kr( 0, 1, trig )
//            val dur		      = LinExp.kr( white, 0, 1, minDur, maxDur )
//            val off0		      = numFrames * white
//            val off		      = off0 - (off0 % 1.0)
//            val gate		      = trig
//            val lFade	      = Latch.kr( fade, trig )
//            val fadeIn	      = lFade * 0.05
//            val fadeOut	      = lFade * 0.15
//            val env 		      = EnvGen.ar( Env.linen( fadeIn, dur, fadeOut, 1, sinShape ), gate, doneAction = 0 )
//            val recLevel0     = env.sqrt
//            val preLevel0     = (1 - env).sqrt
//            val recLevel      = recLevel0 * rec
//            val preLevel      = preLevel0 * (1 - pre) + pre
//            val run           = recLevel > 0
//            RecordBuf.ar( in, bufID, off, recLevel, preLevel, run, 1 )
//            LocalOut.kr( Impulse.kr( 1.0 / (dur + fadeIn + fadeOut ).max( 0.01 )))
//
//      	   val speed      = pspeed.ar
//			   val play		   = PlayBuf.ar( numChannels, bufID, speed, loop = 1 )
//            mix( in, play, pmix )
//      	}
//      }

//      filter( "gain" ) {
//         val pgain   = pAudio( "gain", ParamSpec( -30, 30 ), 0 )
//         val pmix = pMix
//         graph { in =>
//            val amp  = pgain.ar.dbamp
//            val flt  = in * amp
//            mix( in, flt, pmix )
//         }
//      }

//      filter( "gendy" ) {
//         val pamt = pAudio( "amt", ParamSpec( 0, 1 ), 1 )
//         val pmix = pMix
//         graph { in =>
//            val amt     = Lag.ar( pamt.ar, 0.1 )
//            val minFreq	= amt * 69 + 12;
//            val scale	= amt * 13 + 0.146;
//            val gendy   = Gendy1.ar( 2, 3, 1, 1,
//                     minFreq = minFreq, maxFreq = minFreq * 8,
//                     ampScale = scale, durScale = scale,
//                     initCPs = 7, kNum = 7 ) * in
//            val flt	   = Compander.ar( gendy, gendy, 0.7, 1, 0.1, 0.001, 0.02 )
//            mix( in, flt, pmix )
//         }
//      }

//      filter( "m-above" ) {
//         val pthresh = pControl( "thresh", ParamSpec( 1.0e-3, 1.0e-1, ExpWarp ), 1.0e-2 )
//         val pmix = pMix
//         graph { in =>
//            val numChannels   = in.numOutputs
//            val thresh		   = pthresh.kr
//            val env			   = Env( 0.0, List( S( 0.2, 0.0, stepShape ), S( 0.2, 1.0, linShape )))
//            val ramp			   = EnvGen.kr( env )
//            val volume		   = LinLin.kr( thresh, 1.0e-3, 1.0e-1, 32, 4 )
//            val bufIDs        = List.fill( numChannels )( bufEmpty( 1024 ).id )
//            val chain1 		   = FFT( bufIDs, HPZ1.ar( in ))
//            val chain2        = PV_MagAbove( chain1, thresh )
//            val flt			   = LPZ1.ar( volume * IFFT( chain2 )) * ramp
//
//            // account for initial dly
//            val env2          = Env( 0.0, List( S( BufDur.kr( bufIDs ) * 2, 0.0, stepShape ), S( 0.2, 1, linShape )))
//            val wet			   = EnvGen.kr( env2 )
//            val sig			   = (in * (1 - wet).sqrt) + (flt * wet)
//            mix( in, sig, pmix )
//         }
//      }

//      filter( "pitch" ) {
//         val ptrans  = pControl( "shift", ParamSpec( 0.125, 4, ExpWarp ), 1 )
//         val ptime   = pControl( "time",  ParamSpec( 0.01, 1, ExpWarp ), 0.1 )
//         val ppitch  = pControl( "pitch", ParamSpec( 0.01, 1, ExpWarp ), 0.1 )
//         val pmix    = pMix
//         graph { in =>
//            val grainSize  = 0.5f
//            val pitch	   = ptrans.kr
//            val timeDisp	= ptime.kr
//            val pitchDisp	= ppitch.kr
//            val flt		   = PitchShift.ar( in, grainSize, pitch, pitchDisp, timeDisp * grainSize )
//            mix( in, flt, pmix )
//         }
//      }

//      filter( "pow" ) {
//         val pamt = pAudio( "amt", ParamSpec( 0, 1 ), 0.5 )
//         val pmix = pMix
//         graph { in =>
//            val amt  = pamt.ar
//            val amtM = 1 - amt
//            val exp  = amtM * 0.5 + 0.5
//            val flt0 = in.abs.pow( exp ) * in.signum
//            val amp0 = Amplitude.ar( flt0 )
//            val amp  = amtM + (amp0 * amt)
////            val flt  = LeakDC.ar( flt0 ) * amp
//            val flt  = flt0 * amp
//            mix( in, flt, pmix )
//         }
//      }

// XXX this has problems with UDP max datagram size
/*
      filter( "renoise" ) {
         val pcolor  = pAudio( "color", ParamSpec( 0, 1 ), 0 )
         val pmix    = pMix
         val step	   = 0.5
         val freqF   = math.pow( 2, step )
         val freqs	= Array.iterate( 32.0, 40 )( _ * freqF ).filter( _ <= 16000 )
         graph { in =>
            val color         = Lag.ar( pcolor.ar, 0.1 )
            val numChannels   = in.numOutputs
            val noise	      = WhiteNoise.ar( numChannels )
            val sig           = freqs.foldLeft[ GE ]( 0 ){ (sum, freq) =>
               val filt       = BPF.ar( in, freq, step )
               val freq2      = ZeroCrossing.ar( filt )
               val w0         = Amplitude.ar( filt )
               val w2         = w0 * color
               val w1         = w0 * (1 - color)
               sum + BPF.ar( (noise * w1) + (LFPulse.ar( freq2 ) * w2), freq, step )
            }
            val amp           = step.reciprocal  // compensate for Q
            val flt           = sig * amp
            mix( in, flt, pmix )
         }
      }
*/

//      filter( "verb" ) {
//         val pextent = pScalar( "size", ParamSpec( 0, 1 ), 0.5 )
//         val pcolor  = pControl( "color", ParamSpec( 0, 1 ), 0.5 )
//         val pmix    = pMix
//         graph { in =>
//            val extent     = pextent.ir
//            val color	   = Lag.kr( pcolor.kr, 0.1 )
//            val i_roomSize	= LinExp.ir( extent, 0, 1, 1, 100 )
//            val i_revTime  = LinExp.ir( extent, 0, 1, 0.3, 20 )
//            val spread	   = 15
//            val numChannels= in.numOutputs
//            val ins        = in.outputs
//            val verbs      = (ins :+ ins.last).grouped( 2 ).toSeq.flatMap( pair =>
//               (GVerb.ar( Mix( pair ), i_roomSize, i_revTime, color, color, spread, 0, 1, 0.7, i_roomSize ) * 0.3).outputs
//            )
//// !! BUG IN SCALA 2.8.0 : CLASSCASTEXCEPTION
//// weird stuff goin on with UGenIn seqs...
//            val flt: GE     = Vector( verbs.take( numChannels ): _* ) // drops last one if necessary
//            mix( in, flt, pmix )
//         }
//      }

//      filter( "zero" ) {
//         val pwidth	= pAudio( "width", ParamSpec( 0, 1 ), 0.5 )
//         val pdiv 	= pAudio( "div",   ParamSpec( 1, 10, LinWarp, 1 ), 1 )
//         val plag	   = pAudio( "lag",   ParamSpec( 0.001, 0.1, ExpWarp ), 0.01 )
//         val pmix    = pMix
//         graph { in =>
//            val freq		= ZeroCrossing.ar( in ).max( 20 )
//            val width0  = Lag.ar( pwidth.ar, 0.1 )
//            val amp		= width0.sqrt
//            val width	= width0.reciprocal
//            val div		= Lag.ar( pdiv.ar, 0.1 )
//            val lagTime	= plag.ar
//            val pulse   = Lag.ar( LFPulse.ar( freq / div, 0, width ) * amp, lagTime )
//            val flt		= in * pulse
//            mix( in, flt, pmix )
//         }
//      }

      gen( "@" ) { graph { Silent.ar( 1 )}}

      // ---- master ----

      collMaster = (filter( "master+" ) {
         val ins = (0 until NUM_PLATES).map( idx => pAudioIn( "in" + (idx+1) ))
         graph {
            val sig: GE = ins.flatMap( _.ar.outputs )
            sig
         }
      }).make

      plates = PLATE_TRANSITS.zipWithIndex map { tup =>
         val (transit, idx) = tup
         Plate( idx, transit )
      }
      plates.foreach( p => p.initNeighbours(
         { val i = p.id - 2; if( i >= 0 ) Some( plates( i )) else None },
         { val i = p.id + 2; if( i < NUM_PLATES ) Some( plates( i )) else None }))

//      diff( "rec" ) {
//         val pid = pScalar( id, ParamSpec( 0, NUM_PLATES - 1, step = 1 ), 0 )
//         graph { in =>
//            val b = bufRecord( BASE_PATH + File.separator + "rec" + File.separator + "plate" + pid.v.toInt + ".aif",
//               in.numChannels )
//            DiskOut.ar( b.id, in )
//         }
//      }

      pMaster = diff( "semi-master" )({
//         val pmix    = pMix
         val php     = pControl( "hp", ParamSpec( 0, 1, step = 1 ), 0 )
         val psolo   = pControl( "solo", ParamSpec( 0, NUM_PLATES, step = 1 ), 0 )
         val pamp    = pControl( "amp", ParamSpec( (-40).dbamp, 0.dbamp ), (-30).dbamp )
         graph { inPure =>
            val in            = inPure * pamp.kr
            val inChannels    = in.numOutputs
            val outChannels   = masterBus.numChannels
            val hp            = php.kr
            val solo          = psolo.kr
            val soloIdx       = solo - 1
            val isAll         = solo === 0
            val sigSolo       = in * Seq.tabulate( NUM_PLATES )( _ === soloIdx )
            val sigMix: GE    = in * isAll + sigSolo * (1 - isAll)
            val hpIn          = {
               var pan: GE = 0
//               val fact = 2f / inChannels
               val hpChans = 2
               val fact = 2f / (inChannels - 1) * (hpChans - 1) / hpChans
               for( idx <- 0 until inChannels ) {
                  pan += PanAz.ar( hpChans, in \ idx, idx * fact )
               }
               pan * (hpChans.toFloat / inChannels).sqrt
            }
            val sigSoloM      = Mix( sigSolo )
            val hpSolo        = sigSoloM :: sigSoloM :: Nil
            val hpMix         = hpIn * isAll + hpSolo * (1 - isAll)
//            val sig           = (if( inChannels != outChannels ) hpMix else in) * pamp.kr
            require( sigMix.numOutputs == outChannels )

            Out.ar( masterBus.index, sigMix * (1 - hp) )
            Out.ar( headphonesBus.index, hpMix * hp )
         }
      }).make

//      val pComp = diff( "cupola-comp" )( graph { in =>
//         val ctrl = HPF.ar( in, 50 )
////         val cmp  = Compander.ar( in, ctrl, (-12).dbamp, 1, 1.0/3.0 ) * 2
//         val cmp  = Compander.ar( in, ctrl, (-9).dbamp, 1, 1.0/3.0 ) * 2
//         Out.ar( 0, cmp )
//      }).make

      collMaster ~> pMaster
//      pMaster ~> pComp
//      pComp.play
      pMaster.play

      // tablet
      this.f = f

//s.dumpOSC(1)
   }
}