package de.sciss.semi

import java.awt.FlowLayout
import java.awt.event.{ActionEvent, ActionListener}
import Dissemination._
import SemiNuages._
import de.sciss.synth.proc.ProcTxn
import javax.swing.{AbstractButton, JToggleButton, ButtonGroup, JFrame}

class GUI {
   // hp
   // solo

   val frame = new JFrame( "Dissemination" )
   
   {
      val cp   = frame.getContentPane()
      val bg   = new ButtonGroup()
      cp.setLayout( new FlowLayout() )
      val ggHP = new JToggleButton( "HP" )
      ggHP.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            ProcTxn.atomic { implicit tx =>
               pMaster.control( "hp" ).v = if( ggHP.isSelected() ) 1 else 0
            }
         }
      })
      cp.add( ggHP )
      val ggAll = new JToggleButton( "All" )
      ggAll.setSelected( true )
      cp.add( ggAll )
      bg.add( ggAll )
      soloListener( ggAll, 0 )
      for( i <- 0 until NUM_PLATES ) {
         val ggSolo = new JToggleButton( (i + 1).toString )
         cp.add( ggSolo )
         bg.add( ggSolo )
         soloListener( ggSolo, i + 1 )
      }

      frame.setResizable( false )
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