/*
 *  GUI.scala
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

import java.awt.event.{ActionEvent, ActionListener}
import Dissemination._
import SemiNuages._
import de.sciss.synth.proc.ProcTxn
import javax.swing.{JButton, JCheckBox, JLabel, GroupLayout, JPanel, AbstractButton, JToggleButton, ButtonGroup, JFrame}
import java.awt.{Font, BorderLayout, FlowLayout}

class GUI {
  // hp
  // solo

  val frame = new JFrame("Dissemination")

  {
      // --- process control ----
      val procPane      = new JPanel()
      val procLay       = new GroupLayout( procPane )
      procPane.setLayout( procLay )
      procLay.setAutoCreateContainerGaps( true )

      val procLBPlate   = new JLabel( "Plates:" )
      val procGGPlate   = new JCheckBox()
      procGGPlate.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            val onOff = procGGPlate.isSelected
            ProcTxn.atomic { implicit tx =>
               plates.active_=(onOff)
            }
         }
      })

      val procLBSprenger = new JLabel( "Sprenger:" )
      val procGGSprenger = new JCheckBox()
      procGGSprenger.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            val onOff = procGGSprenger.isSelected
            ProcTxn.atomic { implicit tx =>
               sprenger.active_=(onOff)
            }
         }
      })

      val procLBRegen = new JLabel( "Regen:" )
      val procGGRegen = new JCheckBox()
      procGGRegen.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            val onOff = procGGRegen.isSelected
            ProcTxn.atomic { implicit tx =>
               regen.active_=(onOff)
            }
         }
      })

      val procLBZeven = new JLabel( "Zeven:" )
      val procGGZeven = new JCheckBox()
      procGGZeven.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            val onOff = procGGZeven.isSelected
            ProcTxn.atomic { implicit tx =>
               zeven.active_=(onOff)
            }
         }
      })

      val procLBHeli = new JLabel( "Helicopter:" )
      val procGGHeli = new JCheckBox()
      procGGHeli.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            val onOff = procGGHeli.isSelected
            ProcTxn.atomic { implicit tx =>
               helicopter.foreach( _.active_=(onOff))
            }
         }
      })

      val procLBWindspiel = new JLabel( "Windspiel:" )
      val procGGWindspiel = new JCheckBox()
      procGGWindspiel.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            val onOff = procGGWindspiel.isSelected
            ProcTxn.atomic { implicit tx =>
               windspiel.active_=(onOff)
            }
         }
      })

      val procLBLicht = new JLabel( "Licht:" )
      val procGGLicht = new JCheckBox()
      procGGLicht.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            val onOff = procGGLicht.isSelected
            ProcTxn.atomic { implicit tx =>
               licht.active_=(onOff)
            }
         }
      })

      procLay.setHorizontalGroup( procLay.createSequentialGroup()
         .addGroup( procLay.createParallelGroup()
            .addComponent( procLBPlate )
            .addComponent( procLBSprenger )
            .addComponent( procLBRegen )
            .addComponent( procLBZeven )
            .addComponent( procLBHeli )
            .addComponent( procLBWindspiel )
            .addComponent( procLBLicht )
         )
         .addGroup( procLay.createParallelGroup()
            .addComponent( procGGPlate )
            .addComponent( procGGSprenger )
            .addComponent( procGGRegen )
            .addComponent( procGGZeven )
            .addComponent( procGGHeli )
            .addComponent( procGGWindspiel )
            .addComponent( procGGLicht )
         )
      )

      procLay.setVerticalGroup( procLay.createSequentialGroup()
         .addGroup( procLay.createParallelGroup( GroupLayout.Alignment.BASELINE )
            .addComponent( procLBPlate )
            .addComponent( procGGPlate )
         )
         .addGroup( procLay.createParallelGroup( GroupLayout.Alignment.BASELINE )
            .addComponent( procLBSprenger )
            .addComponent( procGGSprenger )
         )
         .addGroup( procLay.createParallelGroup( GroupLayout.Alignment.BASELINE )
            .addComponent( procLBRegen )
            .addComponent( procGGRegen )
         )
         .addGroup( procLay.createParallelGroup( GroupLayout.Alignment.BASELINE )
            .addComponent( procLBZeven )
            .addComponent( procGGZeven )
         )
         .addGroup( procLay.createParallelGroup( GroupLayout.Alignment.BASELINE )
            .addComponent( procLBHeli )
            .addComponent( procGGHeli )
         )
         .addGroup( procLay.createParallelGroup( GroupLayout.Alignment.BASELINE )
            .addComponent( procLBWindspiel )
            .addComponent( procGGWindspiel )
         )
         .addGroup( procLay.createParallelGroup( GroupLayout.Alignment.BASELINE )
            .addComponent( procLBLicht )
            .addComponent( procGGLicht )
         )
      )

      // --- monitoring ----
      val monitorPane = new JPanel( new FlowLayout() )
      val bg   = new ButtonGroup()
      val ggHP = new JToggleButton( "HP" )
      ggHP.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            ProcTxn.atomic { implicit tx =>
               pMaster.control( "hp" ).v_=(if( ggHP.isSelected ) 1 else 0)
            }
         }
      })
      monitorPane.add( ggHP )
      val ggAll = new JToggleButton( "All" )
      ggAll.setSelected( true )
      monitorPane.add( ggAll )
      bg.add( ggAll )
      soloListener( ggAll, 0 )
      for( i <- 0 until NUM_PLATES ) {
         val ggSolo = new JToggleButton( (i + 1).toString )
         monitorPane.add( ggSolo )
         bg.add( ggSolo )
         soloListener( ggSolo, i + 1 )
      }

val ggShutdown = new JButton( "Shutdown" )
ggShutdown.setFont( new Font( "SansSerif", Font.PLAIN, 24 ))
ggShutdown.addActionListener( new ActionListener {
   def actionPerformed( e: ActionEvent ) {
      Dissemination.shutDownComputer()
   }
})

      frame.setResizable( false )
      val cp   = frame.getContentPane
      cp.add( monitorPane, BorderLayout.SOUTH )
      cp.add( procPane, BorderLayout.NORTH )
cp.add( ggShutdown, BorderLayout.CENTER )
      frame.pack()
      frame.setLocation( 10, SCREEN_BOUNDS.height - (frame.getHeight + 10) )
      frame.setVisible( true )
   }

   private def soloListener( b: AbstractButton, idx: Int ) {
      b.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            ProcTxn.atomic { implicit tx =>
               pMaster.control( "solo" ).v_=(idx)
            }
         }
      })
   }
}