package de.sciss
package semi

import java.awt.{Color, Dimension}

object DisseminationPDF extends App {
  import Dissemination._

  guiRun {
    sys.props("apple.laf.useScreenMenuBar") = "true"
    nuagesGUI = { f =>
      val dim   = new Dimension(400, 400)
      val disp  = f.panel.display
      val draw  = pdflitz.Generate.QuickDraw(dim) { g =>
        val prevQ   = disp.isHighQuality
        val prevBg  = disp.getBackground
        disp.setHighQuality(true)  // uses glyph vectors and thus has no probs with the font in the PDF
        disp.setBackground(Color.lightGray)
        disp.paintDisplay(g, dim)
        disp.setHighQuality(prevQ)
        disp.setBackground(prevBg)
      }
      new pdflitz.SaveAction(Seq(draw)).setupMenu(f)
    }
    init()
  }
}