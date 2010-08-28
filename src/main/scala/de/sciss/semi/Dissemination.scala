package de.sciss.semi

import java.awt.EventQueue
import javax.swing.{WindowConstants, JFrame}

object Dissemination {
   def main( args: Array[ String ]) {
      EventQueue.invokeLater( new Runnable { def run = initGUI })
   }

   def initGUI {
      val f = new JFrame( "Dissemination" )
      f.setSize( 200, 200 )
      f.setDefaultCloseOperation( WindowConstants.DISPOSE_ON_CLOSE )
      f.setLocationRelativeTo( null )
      f.setVisible( true )
   }
}