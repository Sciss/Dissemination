package de.sciss.semi

import java.awt.event.KeyEvent
import java.awt.{Toolkit, GraphicsEnvironment}
import javax.swing.{WindowConstants, SwingConstants, JSplitPane, KeyStroke, JFrame}
import de.sciss.scalainterpreter.{LogPane, CodePane, NamedParam, Interpreter, InterpreterPane}

class ScalaInterpreterFrame(support: REPLSupport)
  extends JFrame("Scala Interpreter") {

  val paneCfg = InterpreterPane.Config()
  val intpCfg = Interpreter.Config()
  intpCfg.bindings :+= NamedParam("support", support)
  intpCfg.imports = Seq(
    "math._", "de.sciss.synth._", "de.sciss.synth.ugen._", "de.sciss.synth.swing._", "de.sciss.synth.proc._",
    "de.sciss.synth.proc.DSL._", "de.sciss.semi._", "support._"
  )
  val codeCfg = CodePane.Config()
  codeCfg.text = """// Press '""" + KeyEvent.getKeyModifiersText(txnKeyStroke.getModifiers) + " + " +
    KeyEvent.getKeyText(txnKeyStroke.getKeyCode) +
    """' to execute transactionally.
      | """.stripMargin
  codeCfg.keyMap += txnKeyStroke -> (() => txnExecute())

  val intp      = Interpreter(intpCfg)
  val codePane  = CodePane(codeCfg)
  InterpreterPane.wrap(intp, codePane)

   private val txnKeyStroke = {
      val ms = Toolkit.getDefaultToolkit.getMenuShortcutKeyMask
      KeyStroke.getKeyStroke( KeyEvent.VK_T, ms )
   }

   // ---- constructor ----
   {
      val cp = getContentPane

     //      pane.initialText = pane.initialText +
     //"""// Press '""" + KeyEvent.getKeyModifiersText( txnKeyStroke.getModifiers() ) + " + " +
     //      KeyEvent.getKeyText( txnKeyStroke.getKeyCode() ) + """' to execute transactionally.
     //
     //"""

     //      pane.initialCode = Some(
     //"""
     //import math._
     //import de.sciss.synth._
     //import de.sciss.synth.ugen._
     //import de.sciss.synth.swing._
     //import de.sciss.synth.proc._
     //import de.sciss.synth.proc.DSL._
     //import de.sciss.semi._
     //import support._
     //"""
     //      )

     //     pane.bindingsCreator = Some( (in: Interpreter ) => {
     //         in.bind( "support", classOf[ REPLSupport ].getName, support )
     //      })

     val lp = LogPane()
     lp.makeDefault()
     //      lp.init
     //      pane.out = Some( lp.writer )
     //      Console.setOut( lp.outputStream )
     //      Console.setErr( lp.outputStream )
     //      System.setErr( new PrintStream( lp.outputStream ))

     // pane.customKeyMapActions += txnKeyStroke -> (() => txnExecute())

     // pane.init
     val sp = new JSplitPane(SwingConstants.HORIZONTAL)
     sp.setTopComponent(codePane.component)
     sp.setBottomComponent(lp.component)
     cp.add( sp )
     val b = GraphicsEnvironment.getLocalGraphicsEnvironment.getMaximumWindowBounds
     setSize(b.width / 2, b.height * 7 / 8)
     sp.setDividerLocation(b.height * 2 / 3)
     setLocationRelativeTo(null)
     setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
   }

  private var txnCount = 0

  def txnExecute() {
    codePane.getSelectedTextOrCurrentLine.foreach { txt =>
      val txnId = txnCount
      txnCount += 1
      val txnTxt = """class _txnBody""" + txnId + """( implicit t: ProcTxn ) {
                                                  """ + txt + """
}
val _txnRes""" + txnId + """ = ProcTxn.atomic( implicit t => new _txnBody""" + txnId + """ )
import _txnRes""" + txnId + """._
                            """

      intp.interpret(txnTxt)
    }
  }
}