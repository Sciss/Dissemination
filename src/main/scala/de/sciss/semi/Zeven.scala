/*
 *  Zeven.scala
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
import ugen._
import SemiNuages._
import Dissemination._
import de.sciss.synth.proc.{DSL, ProcTxn, Proc, ExpWarp, ParamSpec}
import DSL._
import Util._

object Zeven {
   val MIN_FILTER_FADE  =   8.0 // s
   val MAX_FILTER_FADE  =  12.0 // s
   val MIN_DUR          =   4.0 // s
   val MAX_DUR          =  20.0 // s
   val FILTER_GAIN      = -21.0 // dB
   val TAPE_GAIN        =   4.5 // dB
   val MIN_TRIG_FREQ    = 1.0 / 180
   val MAX_TRIG_FREQ    = 1.0 / 90
   val TRIG_CHANGE_FREQ = 1.0 / 607
}

class Zeven( idx: Int ) extends ColorLike {
   import Phylet._

   protected def minFade      = MIN_FILTER_FADE
   protected def maxFade      = MAX_FILTER_FADE
   protected def engageFade   = 0.1

   def name = "zeven"
   def exclusives = Set.empty[ String ]
   def trigger : GE = {
      Dust.kr( LFNoise0.kr( TRIG_CHANGE_FREQ ).linexp( -1, 1, MIN_TRIG_FREQ, MAX_TRIG_FREQ ))
   }

   private val urn = new Urn( (0 until marks.size - 1): _* )

   def plate = plates( idx )

   def gen1( implicit tx: ProcTxn ) : Proc = {
      val g = (gen( name ) {
         val pamp = pControl( "amp", ParamSpec( 0.dbamp, 18.dbamp, ExpWarp ), TAPE_GAIN.dbamp )
         val ppos = pScalar( "pos", ParamSpec( 0, 1800), 1 )
         val pdur = pScalar( "dur", ParamSpec( 0.2, 600), 1 )
         graph {
            val path       = AUDIO_PATH + fs + "PhyletischesMuseumGlass080929HPFLoop-M.aif"
            val startFrame = (ppos.v * 44100L).toLong // AudioFileCache.spec( path ).numFrames
            val b          = bufCue( path, startFrame )
            val env        = EnvGen.kr( Env.linen( 0.02, pdur.ir - (0.02 + 0.5), 0.5 ))
            val done       = Done.kr( env )
            done.react( diskDone )
            DiskIn.ar( 1, b.id ) * env * pamp.kr
         }
      }).make
      val idx     = urn.next
      val dur0    = exprand( MIN_DUR, MAX_DUR )
      val start   = marks( idx )
      var idx2    = idx
      var dur     = 0.0
      do {
         idx2  += 1
         val stop = marks( idx2 )
         dur    = (stop - start).toDouble / 44100
      } while( (idx2 < marks.size - 1) && (dur < dur0) )
      g.control( "pos" ).v = start.toDouble / 44100
      g.control( "dur" ).v = dur
      g
   }

   def filter1( implicit tx: ProcTxn ) : Proc = {
      val f = (filter( name + "-trans" ) {
         val pin2    = pAudioIn( "in2" ) // Some( RichBus.audio( Server.default, 1 ))
         val pfade   = pAudio( "fade", ParamSpec( 0, 1 ), 0 )
         graph { in1 =>
            val in2        = pin2.ar
            require( in1.numOutputs == in2.numOutputs )
            val fade       = pfade.ar
//            val bufIDs     = List.fill( in1.numOutputs )( bufEmpty( 1024 ).id )
//            val chain1		= FFT( bufIDs, in1 )
//            val thresh     = A2K.kr( fade ).linexp( 0, 1, 1.0e1, 1.0e-3 )
//            val chain2     = PV_MagBelow( chain1, thresh )
//            val flt			= IFFT( chain2 ) + in2
            val fltGain = A2K.kr( fade ).linlin( 0, 1, 0, FILTER_GAIN )
            var sig  = in1
            sig      = BPeakEQ.ar( sig, 2336, 0.2, fltGain )
            sig      = BPeakEQ.ar( sig, 3709, 0.2, fltGain )
            sig      = BPeakEQ.ar( sig, 5606, 0.2, fltGain )
            sig      = BPeakEQ.ar( sig, 2428, 1.0, fltGain )
            sig + in2
         }
      }).make
      f
   }
}
