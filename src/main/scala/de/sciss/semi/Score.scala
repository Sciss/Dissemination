package de.sciss.semi

import scala.swing.{Swing, Component, Frame, SimpleSwingApplication}
import java.awt.{RenderingHints, Color, Graphics2D}
import Swing._
import de.sciss.file._
import de.sciss.span.Span
import java.awt.geom.Rectangle2D
import javax.swing.WindowConstants

object Score extends SimpleSwingApplication {
  val file = userHome / "Desktop" / "data1.txt"

  lazy val score = readScore(file)

  lazy val top = new Frame {
    title = "Dissemination"
    contents = new Component {
      override protected def paintComponent(g: Graphics2D) {
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON)
        g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE )
        paintScore(g)
      }
    }
    size = (512, 1024)
    centerOnScreen()
    peer.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    open()
  }

  val pixelsPerFrame  = 28.0 / (44100 * 60) // 10.0 / 44100
  val pixelsPerProc   = 48
  val procSpacing     = 16

  def paintScore(g: Graphics2D) {
    val rect = new Rectangle2D.Double()

    score.zipWithIndex.foreach { case (events, idx) =>
      events.foreach {
        case Span(start, stop) =>
          val x1 = idx * (pixelsPerProc + procSpacing)
          val x2 = x1 + pixelsPerProc
          val y1 = start * pixelsPerFrame
          val y2 = stop  * pixelsPerFrame
          rect.setRect(x1, y1, x2 - x1, y2 - y1)
          g.setColor(Color.black)
          g.fill(rect)

        case _ =>
      }
    }
  }

  //windspiel
  //
  //licht
  //
  //sprenger
  //regen
  //
  //phylet
  //apfel
  //zeven

  val procID    = Map("windspiel" -> 1, "licht" -> 2, "sprenger" -> 3, "regen" -> 4, "phylet" -> 5, "apfel" -> 6, "zeven" -> 7)
  val procName  = procID.map(_.swap)
  val procNum   = procName.keys.max + 1

  def readScore(f: File): Vector[Vector[Span.HasStart]] = {
    val lines = io.Source.fromFile(f, "UTF-8").getLines().filter(_.startsWith("<ANA>"))

    var res   = Vector.fill(procNum)(Vector.empty[Span.HasStart])

    lines.foreach { ln =>
      val words = ln.trim.split(' ')
      val frame = words(1).toLong
      words(2) match {
        case "start-proc" =>
          val idx = procID(words(3))
          res = res.updated(idx, res(idx) :+ Span.From(frame))

        case "stop-proc"  =>
          val idx = procID(words(3))
          val init :+ last = res(idx)
          res = res.updated(idx, init :+ Span(last.start, frame))

        case other =>
          println(s"Warning: skipping command '$other'")
      }
    }

    res
  }
}