package de.sciss.semi

import scala.swing.{Swing, Component, Frame, SimpleSwingApplication}
import java.awt.{RenderingHints, Color, Graphics2D}
import Swing._
import de.sciss.file._
import de.sciss.span.Span
import java.awt.geom.{Line2D, GeneralPath, Rectangle2D}
import javax.swing.WindowConstants
import Dissemination.NUM_PLATES

object Score extends SimpleSwingApplication {
  def data      = file("notes") / "data1.txt"
  def drawFades = true

  case class Region(span: Span.HasStart, fadeIn: (Long, Long) = (0L, 0L), fadeOut: Long = 0L, chans: Any = ())

  case class Wind(ch1: Int, ch2: Int)

  type Data = Vector[Vector[Region]]

  lazy val score: Data = readScore(data)

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
  val pixelsPerChan   = pixelsPerProc.toDouble / NUM_PLATES
  val procSpacing     = 16

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

  val procID    = Map(
    "plates" -> 0, "windspiel" -> 1, "licht" -> 2, "sprenger" -> 3,
    "regen"  -> 4, "phylet"    -> 5, "apfel" -> 6, "zeven"    -> 7
  )
  val procName  = procID.map(_.swap)
  val procNum   = procName.keys.max + 1

  val widths  = Map(
    "plates"   -> NUM_PLATES, "windspiel" -> NUM_PLATES, "licht"  -> NUM_PLATES,
    "sprenger" -> 2         , "regen"     -> 2         , "phylet" -> 2,
    "apfel"    -> 2         , "zeven"     -> 2
  )

  val rect  = new Rectangle2D.Double()
  val gp    = new GeneralPath()
  val line  = new Line2D.Double()

  def paintScore(g: Graphics2D) {
    var xoff = 0.0
    score.zipWithIndex.foreach { case (events, idx) =>
      val x01   = xoff
      val name  = procName(idx)
      val w     = pixelsPerProc * widths(name)/NUM_PLATES
      val x02   = x01 + w
      events.foreach {
        case Region(Span(start, stop), fadeIn, fadeOut, chans) =>
          val y1 = start * pixelsPerFrame
          val y2 = stop  * pixelsPerFrame

          def bang(x1: Double, x2: Double) {
            val shp = if (drawFades && (fadeIn != (0L, 0L) || fadeOut != 0L)) {
              gp.reset()
              val yin1 = (start + fadeIn._1) * pixelsPerFrame
              val yin2 = (start + fadeIn._2) * pixelsPerFrame
              val yout = (stop  - fadeOut  ) * pixelsPerFrame
              val xm   = (x1 + x2) * 0.5
              gp.moveTo(xm, y1)
              gp.lineTo(x1, yin1)
              gp.lineTo(x1, yout)
              gp.lineTo(xm, y2)
              gp.lineTo(x2, yout)
              gp.lineTo(x2, yin2)
              gp.closePath()
              gp

            } else {
              rect.setRect(x1, y1, x2 - x1, y2 - y1)
              rect
            }
            g.setColor(Color.black)
            g.fill(shp)
          }

          chans match {
            case Wind(ch1, ch2) =>
              val x11 = x01 + ch1 * pixelsPerChan
              val x12 = x11 + pixelsPerChan
              val x21 = x01 + ch2 * pixelsPerChan
              val x22 = x21 + pixelsPerChan
              bang(x11, x12)
              bang(x21, x22)
              line.setLine(math.min(x11, x21), y1, math.max(x12, x22), y1)
              g.draw(line)

            case _ =>
              bang(x01, x02)
          }

        case _ =>
      }

      xoff += w + procSpacing
    }
  }

  def readScore(f: File): Data = {
    val lines = io.Source.fromFile(f, "UTF-8").getLines().filter(_.startsWith("<ANA>"))

    var res   = Vector.fill(procNum)(Vector.empty[Region])

    lines.foreach { ln =>
      val words = ln.trim.split(' ')
      val frame = words(1).toLong
      words(2) match {
        case "start-proc" =>
          val name  = words(3)
          if (name != "windspiel") {
            val idx   = procID(name)
            res = res.updated(idx, res(idx) :+ Region(Span.From(frame)))
          }

        case "windspiel-play" =>
          require(words(5) == "idx1" && words(7) == "idx2" && words(13) == "dur")
          val ch1 = words(6).toInt
          val ch2 = words(8).toInt
          val dur = words(14).toLong
          val idx = procID("windspiel")
          res = res.updated(idx, res(idx) :+ Region(Span(frame, frame + dur), chans = Wind(ch1, ch2)))

        case "stop-proc" =>
          val name  = words(3)
          if (/* name == "windspiel" || */ name == "licht") {
            val idx   = procID(name)
            val init :+ (last: Region) = res(idx)
            res = res.updated(idx, init :+ last.copy(span = Span(last.span.start, frame)))
          }

        case "fade-in" =>
          val dur   = words(3).toLong
          val name  = words(4)
          if (name != "licht") {
            val idx   = procID(name)
            val init :+ (last: Region) = res(idx)
            res = res.updated(idx, init :+ last.copy(fadeIn = (dur, dur)))
          }

        case "fade-in-channel" =>
          val ch    = words(3).toInt
          val dur   = words(4).toLong
          val name  = words(5)
          val idx   = procID(name)
          val init :+ (last: Region) = res(idx)
          val oldFade = last.fadeIn
          val newFade = if (ch == 0) oldFade.copy(_1 = dur) else oldFade.copy(_2 = dur)
          res = res.updated(idx, init :+ last.copy(fadeIn = newFade))

        case "fade-out" =>
          val dur   = words(3).toLong
          val name  = words(4)
          val idx   = procID(name)
          val init :+ (last: Region) = res(idx)
          res = res.updated(idx, init :+ last.copy(span = Span(last.span.start, frame + dur), fadeOut = dur))

        case "fade-out-channel" =>
          val ch    = words(3).toInt
          if (ch == 0) {
            val dur   = words(4).toLong
            val name  = words(5)
            val idx   = procID(name)
            val init :+ (last: Region) = res(idx)
            res = res.updated(idx, init :+ last.copy(span = Span(last.span.start, frame + dur), fadeOut = dur))
          }

        case other =>
          println(s"Warning: skipping command '$other'")
      }
    }

    res
  }
}