/*
 *  Material.scala
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

import collection.breakOut

/**
 * @version 0.11, 17-Aug-10
 */
object Material {
   val all = Vector(
      SoundContext( "tp_02ftvc",
         TapeSoundSettings( "02FTVcdAmpFTAmp.aif", 0.0, 1.0 ),
         0.0, 0.5, 1.5, 2, 6, 45.0, 120.0, 20.0, 40.0, Set.empty
      ),
      SoundContext( "tp_13ftvc",
         TapeSoundSettings( "13FTVcdAmpFTAmp.aif", 0.0, 1.0 ),
         0.0, 0.5, 1.5, 2, 6, 45.0, 120.0, 20.0, 40.0, Set.empty
      ),
      SoundContext( "tp_15ftvc",
         TapeSoundSettings( "15FTVcdAmpFTAmp.aif", 0.0, 1.0 ),
         0.0, 0.5, 1.5, 2, 6, 45.0, 120.0, 20.0, 40.0, Set.empty
      ),
      SoundContext( "tp_18ftvc",
         TapeSoundSettings( "18FTVcdAmpFTAmp.aif", 0.0, 1.0 ),
         0.0, 0.5, 1.5, 2, 6, 45.0, 120.0, 20.0, 40.0, Set.empty
      ),
      SoundContext( "tp_24ftvc",
         TapeSoundSettings( "24FTVcdAmpFTAmp.aif", 0.0, 1.0 ),
         0.0, 0.5, 1.5, 2, 6, 45.0, 120.0, 20.0, 40.0, Set.empty
      ),
      SoundContext( "tp_25ftvc",
         TapeSoundSettings( "25FTVcdAmpFTAmp.aif", 0.0, 1.0 ),
         0.0, 0.5, 1.5, 2, 6, 45.0, 120.0, 20.0, 40.0, Set.empty
      )
   )

   val map: Map[ String, SoundContext ] = all.map( c => (c.name -> c) )( breakOut )
}