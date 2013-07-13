package de.sciss
package semi

import scala.swing.{Frame, SimpleSwingApplication}

object MakePDF extends SimpleSwingApplication {
  lazy val top: Frame = {
    sys.props("apple.laf.useScreenMenuBar") = "true"
    val res = Score.top
    new pdflitz.SaveAction(Seq(res.contents.head)).setupMenu(res)
    res
  }
}