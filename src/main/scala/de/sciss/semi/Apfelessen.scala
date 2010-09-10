/*
 *  Apfelessen.scala
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

import de.sciss.synth
import synth._
import proc.{DSL, RichBus, ExpWarp, ParamSpec, Proc, ProcTxn}
import ugen._
import Util._
import SemiNuages._
import DSL._
import Dissemination._

object Apfelessen {
//   val GEN_FADE_IN      = 0.1
//   val GEN_FADE_IN      = 0.1
   val MIN_FILTER_FADE  = 10
   val MAX_FILTER_FADE  = 10

   val marks = Vector( 0, 508517, 906299, 1460239, 1880865, 2316152, 2720615, 3154876, 3696601,
      4239869, 4730878, 5221358, 6041236, 6706778, 7263365, 7781584, 8522199, 8809907, 9796204,
      10825630, 11753000, 12669291, 13662245, 14059904, 14553250, 15014272, 15412362, 15793122,
      16214938, 16568312, 16883715, 17288024, 17632268, 17938895, 18246999, 18590891, 18944132,
      19282908, 20034970, 20509064, 20905410, 21265568, 22108634, 22343952, 22685859, 23053207 )
}

class Apfelessen( idx: Int ) extends ColorLike {
   import Apfelessen._
   
   protected def minFade      = MIN_FILTER_FADE
   protected def maxFade      = MAX_FILTER_FADE
   protected def engageFade   = 0.1

   def name = "apfel"
   def exclusives = Set.empty[ String ] // Set( "regen" )
   def trigger : GE = {
      Dust.kr( LFNoise0.kr.linexp( -1, 1, 1.0/70, 1.0/140 ))
   }

//   def init( implicit tx: ProcTxn ) {
//      gen( "apfel" ) {
//         val ppos =
//      }
//   }

   def plate = plates( idx )

   def gen1( implicit tx: ProcTxn ) : Proc = {
      val g = (gen( "apfel" ) {
         val pamp = pControl( "amp", ParamSpec( 0.dbamp, 18.dbamp, ExpWarp ), 3.dbamp )
         val ppos = pScalar( "pos", ParamSpec( 0, 600), 1 )
         val pdur = pScalar( "dur", ParamSpec( 0.2, 600), 1 )
         graph {
            val path       = AUDIO_PATH + fs + "ApflssnConAtemAmp-M.aif"
            val startFrame = (ppos.v * 44100L).toLong // AudioFileCache.spec( path ).numFrames
            val b          = bufCue( path, startFrame )
            val env        = EnvGen.kr( Env.linen( 0.02, pdur.ir - (0.02 + 0.5), 0.5 ))
            val done       = Done.kr( env )
            done.react( diskDone )
            DiskIn.ar( 1, b.id ) * env * pamp.kr
         }
      }).make
      val idx     = rand( marks.size - 1 )
      val start   = marks( idx )
      val stop    = marks( idx + 1 )
      g.control( "pos" ).v = start.toDouble / 44100
      g.control( "dur" ).v = (stop - start).toDouble / 44100
      g
   }

   def filter1( implicit tx: ProcTxn ) : Proc = {
      val f = (filter( "apfel-trans" ) {
         val pin2    = pAudioIn( "in2" ) // Some( RichBus.audio( Server.default, 1 ))
         val pfade   = pAudio( "fade", ParamSpec( 0, 1 ), 0 )
         graph { in1 =>
            val in2  = pin2.ar
            require( in1.numOutputs == in2.numOutputs )
            val fade    = pfade.ar

            val bufIDs        = List.fill( in1.numOutputs )( bufEmpty( 1024 ).id )
            val chain1		   = FFT( bufIDs, in1 )
            val thresh        = A2K.kr( fade ).linexp( 0, 1, 1.0e-3, 1.0e1 )
            val chain2        = PV_MagAbove( chain1, thresh )
            val flt			   = IFFT( chain2 ) + in2
            flt
         }
      }).make
      f
   }
}