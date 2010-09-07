package de.sciss.semi

import java.awt.event.{ActionEvent, ActionListener}
import Dissemination._
import SemiNuages._
import de.sciss.synth.proc.ProcTxn
import java.awt.{BorderLayout, FlowLayout}
import javax.swing.{JCheckBox, JLabel, GroupLayout, JPanel, AbstractButton, JToggleButton, ButtonGroup, JFrame}

class GUI {
   // hp
   // solo

   val frame = new JFrame( "Dissemination" )
   
   {
      // --- process control ----
      val procPane      = new JPanel()
      val procLay       = new GroupLayout( procPane )
      procPane.setLayout( procLay )
      procLay.setAutoCreateContainerGaps( true )

      val procLBPlate   = new JLabel( "Plate:" )
      val procGGPlate   = new JCheckBox()
      procGGPlate.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            val onOff = procGGPlate.isSelected()
            ProcTxn.atomic { implicit tx =>
               plates.foreach( _.active = onOff )
            }
         }
      })

      val procLBSprenger = new JLabel( "Sprenger:" )
      val procGGSprenger = new JCheckBox()
      procGGSprenger.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            val onOff = procGGSprenger.isSelected()
            ProcTxn.atomic { implicit tx =>
               sprenger.active = onOff
            }
         }
      })

      val procLBRegen = new JLabel( "Regen:" )
      val procGGRegen = new JCheckBox()
      procGGRegen.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            val onOff = procGGRegen.isSelected()
            ProcTxn.atomic { implicit tx =>
               regen.active = onOff
            }
         }
      })

      procLay.setHorizontalGroup( procLay.createSequentialGroup()
         .addGroup( procLay.createParallelGroup()
            .addComponent( procLBPlate )
            .addComponent( procLBSprenger )
            .addComponent( procLBRegen )
         )
         .addGroup( procLay.createParallelGroup()
            .addComponent( procGGPlate )
            .addComponent( procGGSprenger )
            .addComponent( procGGRegen )
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
      )

      // --- monitoring ----
      val monitorPane = new JPanel( new FlowLayout() )
      val bg   = new ButtonGroup()
      val ggHP = new JToggleButton( "HP" )
      ggHP.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            ProcTxn.atomic { implicit tx =>
               pMaster.control( "hp" ).v = if( ggHP.isSelected() ) 1 else 0
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

      frame.setResizable( false )
      val cp   = frame.getContentPane()
      cp.add( monitorPane, BorderLayout.SOUTH )
      cp.add( procPane, BorderLayout.NORTH )
      frame.pack()
      frame.setLocation( 10, SCREEN_BOUNDS.height - (frame.getHeight() + 10) )
      frame.setVisible( true )
   }

   private def soloListener( b: AbstractButton, idx: Int ) {
      b.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            ProcTxn.atomic { implicit tx =>
               pMaster.control( "solo" ).v = idx
            }
         }
      })
   }
}