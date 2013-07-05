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

object Phylet {
   val MIN_FILTER_FADE  =   8.0 // s
   val MAX_FILTER_FADE  =  12.0 // s
   val MIN_DUR          =   4.0 // s
   val MAX_DUR          =  20.0 // s
   val FILTER_GAIN      = -21.0 // dB
   val TAPE_GAIN        =   4.5 // dB
   val MIN_TRIG_FREQ    = 1.0 / 180
   val MAX_TRIG_FREQ    = 1.0 / 90
   val TRIG_CHANGE_FREQ = 1.0 / 607

   val marks = Vector(
        245877,   497303,   632686,   930528,  1313469,  1429512,  1572631,  1785376,  2013593,  2165715,  2566061,
       2825223,  2958671,  3137186,  3292251,  3492503,  3866717,  4287319,  4497861,  5359160,  5680179,  6130048,
       6352369,  6524499,  6728015,  7140906,  7389387,  7462881,  7615670,  7778482,  7953098,  8198363,  8341379,
       8519587,  8656738,  8775191,  8852763,  8997190,  9077496,  9228142,  9360795,  9569042,  9671134,  9776400,
       9903452, 10078574, 10211359, 10337088, 10452497, 10683185, 10799917, 11004277, 11510971, 11915324, 12273178,
      12607588, 12870380, 13029581, 13331975, 13456028, 13781505, 13872043, 14093866, 14162485, 14448738, 14664040,
      14931065, 15052296, 15292906, 15532084, 15605744, 15696413, 15772133, 16254763, 16540777, 16755323, 16887203,
      17095928, 17340783, 17470790, 17744136, 17838334, 17898221, 17998725, 18146242, 18366566, 18557916, 18715485,
      19202965, 19535700, 19989709, 20090434, 20315123, 20430357, 20575044, 20709937, 20870461, 21039938, 21235301,
      21314019, 21425099, 21808123, 21905297, 22106173, 22447146, 22499061, 22868842, 23004670, 23216305, 23364446,
      23542610, 23679957, 23991730, 24347330, 24460217, 24907920, 25229223, 25630940, 26200309, 26419914, 26650734,
      26785062, 26991053, 27297845, 27382749, 27495454, 27617920, 27813415, 27940247, 28118499, 28280478, 28934613,
      29038204, 29150615, 29399030, 29744298, 30288889, 30370651, 30459777, 30576642, 30702106, 31293102, 31506193,
      31654899, 31729384, 31938021, 32022649, 32263611, 32438556, 33000875, 33393718, 33749666, 33976384, 34669636,
      35409313, 35494914, 36035972, 36170257, 36658897, 36789122, 37188902, 37426185, 37683744, 37904553, 38350348,
      38678716, 38930968, 39025783, 39206849, 39270407, 39535727, 39680405, 39795192, 39919334, 40177760, 40260732,
      41043309, 41113643, 41207091, 41291763, 41441614, 41502913, 42117368, 42322654, 42669088, 42923504, 43154016,
      43292799, 43479871, 44522836, 44863099, 44928401, 44991111, 45103478, 45662108, 45887238, 46437606, 46821748,
      47241108, 47395899, 47743892, 47880999, 48075215, 48273710, 48642871, 48795174, 49061008, 49306072, 49651860,
      49826981, 50274607, 50627804, 50764823, 50904840, 51074052, 51217157, 51403920, 51840775, 52011265, 52141316,
      52323714, 52477005, 52628842, 52764581, 53113721, 53240773, 53402841, 53525351, 53601247, 53938479, 54097592,
      54250663, 54601038, 54677910, 54828594, 54881019, 54959825, 55037177, 55353767, 55965037, 56362372, 56464640,
      56736384, 56851996, 57043125, 57257628, 57398503, 57862644, 58256737, 58348685, 58441472, 58603832, 58746148,
      59063637, 59505875, 59629708, 59689684, 59876492, 59914506, 60126407, 60278111, 60354051, 60449086, 60513340,
      60778866, 60870682, 61063311, 61165447, 61482813, 61783926, 61998932, 62090969, 62163954, 62232574, 62394685,
      62547889, 62674941, 62774386, 62962781, 63132847, 63173620 )
}

class Phylet(val idx: Int) extends ColorLike {

  import Phylet._

  protected def minFade     = MIN_FILTER_FADE
  protected def maxFade     = MAX_FILTER_FADE
  protected def engageFade  = 0.1
  protected def delayGen    = true

  def name = "phylet"

  def exclusives = Set.empty[String]

  def trigger: GE = {
    Dust.kr(LFNoise0.kr(TRIG_CHANGE_FREQ).linexp(-1, 1, MIN_TRIG_FREQ, MAX_TRIG_FREQ))
  }

  private val urn = new Urn(0 until marks.size - 1: _*)

   def gen1( implicit tx: ProcTxn ) : Proc = {
      val g = (ProcDemiurg.factories.find( _.name == name ) getOrElse gen( name ) {
         val pamp = pControl( "amp", ParamSpec( 0.dbamp, 18.dbamp, ExpWarp ), TAPE_GAIN.dbamp )
         val ppos = pScalar( "pos", ParamSpec( 0, 1800), 1 )
         val pdur = pScalar( "dur", ParamSpec( 0.2, 600), 1 )
         graph {
            val path       = AUDIO_PATH + fs + "PhyletischesMuseumGlass080929HPFLoop-M.aif"
            val startFrame = (ppos.v * 44100L).toLong // AudioFileCache.spec( path ).numFrames
            val b          = bufCue( path, startFrame )
            val env        = EnvGen.kr( Env.linen( 0.02, pdur.ir - (0.02 + 0.5), 0.5 ))
            val done       = Done.kr( env )
            done.react( diskDone() )
            DiskIn.ar( 1, b.id ) * env * pamp.kr
         }
      }).make
      val idx     = urn.next()
      val dur0    = exprand( MIN_DUR, MAX_DUR )
      val start   = marks( idx )
      var idx2    = idx
      var dur     = 0.0
      do {
         idx2  += 1
         val stop = marks( idx2 )
         dur    = (stop - start).toDouble / 44100
      } while( idx2 < marks.size - 1 && dur < dur0 )
     val pos = start.toDouble / 44100
      g.control( "pos" ).v_=(pos)
      g.control( "dur" ).v_=(dur)

    Analysis.log(s"$name-gen1 idx $idx pos ${(pos * 44100L).toLong} dur ${(dur * 44100L).toLong}")
     g
   }

   def filter1( implicit tx: ProcTxn ) : Proc = {
      val fltName = name + "-trans"
      val f = (ProcDemiurg.factories.find( _.name == fltName ) getOrElse filter( fltName ) {
         val pin2    = pAudioIn( "in2" ) // Some( RichBus.audio( Server.default, 1 ))
         val pfade   = pAudio( "fade", ParamSpec( 0, 1 ), 0 )
         graph { in1: In =>
            val in2        = pin2.ar
            require( in1.numChannels /* .numOutputs */ == in2.numChannels /* .numOutputs */)
            val fade       = pfade.ar
//            val bufIDs     = List.fill( in1.numOutputs )( bufEmpty( 1024 ).id )
//            val chain1		= FFT( bufIDs, in1 )
//            val thresh     = A2K.kr( fade ).linexp( 0, 1, 1.0e1, 1.0e-3 )
//            val chain2     = PV_MagBelow( chain1, thresh )
//            val flt			= IFFT( chain2 ) + in2
            val fltGain = A2K.kr( fade ).linlin( 0, 1, 0, FILTER_GAIN )
            var sig: GE  = in1
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
