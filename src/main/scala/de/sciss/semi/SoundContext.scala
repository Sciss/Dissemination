/*
 *  SoundContext.scala
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

import collection.{ Set => ISet }
import _root_.de.sciss.synth._
import ugen._
import proc._
import Util._
import Dissemination._
import java.io.File

case class SoundContext( name: String, settings: SoundSettings,
                         scaleStart: Double, scaleStop: Double, weight: Double,
                         minConc: Int, maxConc: Int,
                         minDur: Double, maxDur: Double,
                         minFade: Double, maxFade: Double,
                         mutex: ISet[ String ])

sealed abstract class SoundSettings {
//   def toXML : Node
   def createProcFactory( name: String )( implicit tx: ProcTxn ) : ProcFactory
   def prepareForPlay( proc: Proc )( implicit tx: ProcTxn ) : Unit
}

trait TapeSoundSettingsLike extends SoundSettings {
   def file: String
   def gain: Double
   def speed: Double

   def fullPath : String

   def createProcFactory( name: String )( implicit tx: ProcTxn ) : ProcFactory = {
      import DSL._
      ProcDemiurg.factories.find( _.name == name ) getOrElse gen( name ) {
         val pspeed  = pControl( "speed", ParamSpec( 0.1f, 10, ExpWarp ), speed )
//         val pamp    = pControl( "amp",   ParamSpec( 0.1f, 10, ExpWarp ), gain.dbamp )
         val pamp    = pAudio( "amp",   ParamSpec( 0.001, 10, ExpWarp ), gain.dbamp )
         val ppos    = pScalar(  "pos",   ParamSpec( 0, 1 ), 0 )
         graph {
//            val fullPath   = BASE_PATH + File.separator + "audio_work" + File.separator + file
            val afSpec     = audioFileSpec( fullPath )
            val startPos   = ppos.v
            val startFrame = (startPos * afSpec.numFrames).toLong
            val buf        = bufCue( fullPath, startFrame )
            val bufID      = buf.id
            val speed      = pspeed.kr * BufRateScale.ir( bufID )
            val d          = Mix( VDiskIn.ar( afSpec.numChannels, bufID, speed, loop = 1 )) // Mix!
//               val frame   = d.reply
//               (frame.carry( pspeed.v * b.sampleRate ) / b.numFrames) ~> ppos
//            val liveFrame  = Integrator.ar( K2A.ar( speed ))
//            val livePos    = ((liveFrame / BufFrames.ir( bufID )) + startPos) % 1.0f
//               livePos ~> ppos
//            d * pamp.kr
            d * pamp.ar
//val env = EnvGen.kr( Env( 1, EnvSeg( 1, 0 ) :: Nil, 0 ), "gate".kr( 1 ))
         }
      }
   }

   def prepareForPlay( proc: Proc )( implicit tx: ProcTxn ) {
      proc.control( "pos" ).v = rand( 0.95 )
   }
}

case class TapeSoundSettings( file: String, gain: Double, speed: Double )
extends TapeSoundSettingsLike {
   def fullPath = BASE_PATH + File.separator + "audio_work" + File.separator + file
}

case class InjectSoundSettings( fullPath: String, file: String, gain: Double, speed: Double )
extends TapeSoundSettingsLike

object SimpleFilterSettings extends SoundSettings {
   def createProcFactory( name: String )( implicit tx: ProcTxn ) : ProcFactory = {
      ProcDemiurg.factories.find( _.name == name ) getOrElse error( "Invalid filter: " + name )
   }

   def prepareForPlay( proc: Proc )( implicit tx: ProcTxn ) {}
}

object VerbFilterSettings extends SoundSettings {
   def createProcFactory( name: String )( implicit tx: ProcTxn ) : ProcFactory = {
      ProcDemiurg.factories.find( _.name == name ) getOrElse error( "Invalid filter: " + name )
   }

   def prepareForPlay( proc: Proc )( implicit tx: ProcTxn ) {
      proc.control( "size" ).v = rrand( 0.7, 1.0 )
   }
}