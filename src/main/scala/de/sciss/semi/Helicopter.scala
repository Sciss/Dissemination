/*
 *  Phylet.scala
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
import proc.{ProcDemiurg, DSL, ProcTxn, Proc, ExpWarp, ParamSpec}
import ugen._
import SemiNuages._
import Dissemination._
import DSL._
import Util._

object Helicopter {
   val MIN_FILTER_FADE  =   8.0 // s
   val MAX_FILTER_FADE  =  12.0 // s
   val MIN_DUR          =   4.0 // s
   val MAX_DUR          =  20.0 // s
   val FILTER_GAIN      = -21.0 // dB
   val TAPE_GAIN        =   3.0 // dB
   val MIN_TRIG_FREQ    = 1.0 / 180
   val MAX_TRIG_FREQ    = 1.0 / 90
   val TRIG_CHANGE_FREQ = 1.0 / 607
}

class Helicopter( idx: Int ) extends ColorLike {
   import Helicopter._

   protected def minFade      = MIN_FILTER_FADE
   protected def maxFade      = MAX_FILTER_FADE
   protected def engageFade   = 0.1
   protected def delayGen     = true

   def name = "Helicopter"
   def exclusives = Set.empty[ String ]
   def trigger : GE = {
      Dust.kr( LFNoise0.kr( TRIG_CHANGE_FREQ ).linexp( -1, 1, MIN_TRIG_FREQ, MAX_TRIG_FREQ ))
   }

   def plate = plates( idx )

   def gen1( implicit tx: ProcTxn ) : Proc = {
      val g = (ProcDemiurg.factories.find( _.name == name ) getOrElse gen( name ) {
         val pamp = pControl( "amp", ParamSpec( 0.dbamp, 18.dbamp, ExpWarp ), TAPE_GAIN.dbamp )
         val ppos = pScalar( "pos", ParamSpec( 0, 1800), 1 )
         val pdur = pScalar( "dur", ParamSpec( 0.2, 600), 1 )
         graph {
            val path       = AUDIO_PATH + fs + "Ahnng'tEdCtOpAhnng'lp100-M.aif"
            val startFrame = (ppos.v * 44100L).toLong // AudioFileCache.spec( path ).numFrames
            val b          = bufCue( path, startFrame )
            val env        = EnvGen.kr( Env.linen( 0.02, pdur.ir - (0.02 + 2.0), 2.0 ))
            val done       = Done.kr( env )
            done.react( diskDone )
            DiskIn.ar( 1, b.id, loop = 1 ) * env * pamp.kr
         }
      }).make
      g.control( "pos" ).v = rrand( 0.0, 117.0 )
      g.control( "dur" ).v = exprand( MIN_DUR, MAX_DUR ) + 2.0
      g
   }

   def filter1( implicit tx: ProcTxn ) : Proc = {
      val fltName = name + "-trans"
      val f = (ProcDemiurg.factories.find( _.name == fltName ) getOrElse filter( fltName ) {
         val pin2    = pAudioIn( "in2" ) // Some( RichBus.audio( Server.default, 1 ))
         val pfade   = pAudio( "fade", ParamSpec( 0, 1 ), 0 )
         graph { in1: In =>
            val in2  = pin2.ar
            require( in1.numChannels /* .numOutputs */ == in2.numChannels /* .numOutputs */)
            val fade    = pfade.ar

            val bufIDs        = List.fill( in1.numChannels /* .numOutputs */)( bufEmpty( 1024 ).id )
            val chain1		   = FFT( bufIDs, in1 )
            val thresh        = A2K.kr( fade ).linexp( 1, 0, 4.0e-2, 1.0e1 )
            val chain2        = PV_MagBelow( chain1, thresh )
            val flt			   = IFFT( chain2 ) + in2
            flt
         }
      }).make
      f
   }
}
