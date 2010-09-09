package de.sciss.semi

import de.sciss.scalainterpreter.{ LogPane, ScalaInterpreterPane }
import tools.nsc.Interpreter
import java.io.PrintStream
import java.awt.event.KeyEvent
import java.awt.{Toolkit, GraphicsEnvironment}
import javax.swing._

/**
 *    @version 0.11, 04-Jun-10
 */
class ScalaInterpreterFrame( support: REPLSupport )
extends JFrame( "Scala Interpreter" ) {
   val pane = new ScalaInterpreterPane

   private val txnKeyStroke = {
      val ms = Toolkit.getDefaultToolkit.getMenuShortcutKeyMask
      KeyStroke.getKeyStroke( KeyEvent.VK_T, ms )
   }

   // ---- constructor ----
   {
      val cp = getContentPane

      pane.initialText = pane.initialText +
"""// Press '""" + KeyEvent.getKeyModifiersText( txnKeyStroke.getModifiers() ) + " + " +
      KeyEvent.getKeyText( txnKeyStroke.getKeyCode() ) + """' to execute transactionally.

val so = new ServerOptionsBuilder
so.nrtOutputPath = "/Users/hhrutz/Desktop/test.aiff"
so.sampleRate = 44100
so.outputBusChannels = 2

import Float.{ PositiveInfinity => inf }
case class Span( start: Long, stop: Long ) { def length = stop - start }

val path = "/Users/hhrutz/Documents/Cupola/rec/Windspiel2.aif"

val bc = BounceSynthContext( so )

import Util._

val df3 = SynthDef( "WindVoice" ) {
   val bufID     = "buf".ir
   val rate      = "rate".kr( 1 )
   val dur       = "dur".ir
   val play      = VDiskIn.ar( 2, bufID, rate )
   val env       = Line.ar( 1, 0, dur, freeSelf )
   Out.ar( 0, play * env )
}
bc.perform( bc.add( df3.recvMsg ), 0.0 )

val w = ProcDemiurg.worlds(s)
val fact = ProcDemiurg.factories
val test = fact.find( _.name == "test" ).get
val n = Dissemination.NUM_PLATES
val tests = (0 until n).map( i => {
    val p = test.make
    p.control( "phas" ).v = i.linlin( 0, n + 1, 1, 0 )
    p.control( "f1" ).v = 0.4
    p.control( "f2" ).v = (i * 2 + 64).midicps
    p.control( "q" ).v  = 50
    p ~> SemiNuages.plates( i ).collector
    p.play
    p
})
"""

      pane.initialCode = Some(
"""
import math._
import de.sciss.synth._
import de.sciss.synth.ugen._
import de.sciss.synth.swing._
import de.sciss.synth.proc._
import de.sciss.synth.proc.DSL._
import de.sciss.semi._
import support._
"""
      )

      pane.bindingsCreator = Some( (in: Interpreter ) => {
         in.bind( "support", classOf[ REPLSupport ].getName, support )
      })

      val lp = new LogPane
      lp.init
      pane.out = Some( lp.writer )
      Console.setOut( lp.outputStream )
      Console.setErr( lp.outputStream )
      System.setErr( new PrintStream( lp.outputStream ))

      pane.customKeyMapActions += txnKeyStroke -> (() => txnExecute)

      pane.init
      val sp = new JSplitPane( SwingConstants.HORIZONTAL )
      sp.setTopComponent( pane )
      sp.setBottomComponent( lp )
      cp.add( sp )
      val b = GraphicsEnvironment.getLocalGraphicsEnvironment.getMaximumWindowBounds
      setSize( b.width / 2, b.height * 7 / 8 )
      sp.setDividerLocation( b.height * 2 / 3 )
      setLocationRelativeTo( null )
      setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
   }

   private var txnCount = 0

   def txnExecute {
      pane.getSelectedTextOrCurrentLine.foreach( txt => {
         val txnId  = txnCount
         txnCount += 1
         val txnTxt = """class _txnBody""" + txnId + """( implicit t: ProcTxn ) {
""" + txt + """
}
val _txnRes""" + txnId + """ = ProcTxn.atomic( implicit t => new _txnBody""" + txnId + """ )
import _txnRes""" + txnId + """._
"""

         pane.interpret( txnTxt )
      })
   }
}