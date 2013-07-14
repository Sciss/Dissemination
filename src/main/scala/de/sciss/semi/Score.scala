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
  def data            = file("notes") / "data2.txt"
  def drawFades       = true
  def drawRecordConn  = false // too messy

  case class Region(span: Span.HasStart, fadeIn: (Long, Long) = (0L, 0L), fadeOut: (Long, Long) = (0L, 0L), detail: Any = ())

  case class Wind(ch1: Int, ch2: Int)

  case object Light // class Light(isLTR: Boolean)

  case class Record(chan: Int, span: Span)

  case class Inject(chan: Int, source: Boolean, name: String, rec: Option[Record])

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
    size = (400, 1024)
    centerOnScreen()
    peer.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    open()
  }

  // i.e. 26 pixels per minute, with 1024 pixels height, yielding roughly 40 minutes (39.38)
  val pixelsPerFrame  = 26.0 / (44100 * 60) // 10.0 / 44100
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
    "plates"   -> NUM_PLATES * 2, "windspiel" -> NUM_PLATES, "licht"  -> NUM_PLATES,
    "sprenger" -> 2             , "regen"     -> 2         , "phylet" -> 2,
    "apfel"    -> 2             , "zeven"     -> 2
  )

  val rect  = new Rectangle2D.Double()
  val gp    = new GeneralPath()
  val line  = new Line2D.Double()

  def paintScore(g: Graphics2D) {
    var xoff = procSpacing * 0.5
    score.zipWithIndex.foreach { case (events, idx) =>
      val x01   = xoff
      val name  = procName(idx)
      val w     = pixelsPerProc * widths(name)/NUM_PLATES
      val x02   = x01 + w
      events.foreach {
        case Region(Span(start, stop), fadeIn, fadeOut, chans) =>
          val y1 = start * pixelsPerFrame
          val y2 = stop  * pixelsPerFrame

          def bang(x1: Double, x2: Double, xw1: Double = 0.5, xw2: Double = 0.5, fill: Boolean = true) {
            val shp = if (drawFades && (fadeIn != (0L, 0L) || fadeOut != (0L, 0L))) {
              gp.reset()
              val yin1  = (start + fadeIn ._1) * pixelsPerFrame
              val yin2  = (start + fadeIn ._2) * pixelsPerFrame
              val yout1 = (stop  - fadeOut._1) * pixelsPerFrame
              val yout2 = (stop  - fadeOut._2) * pixelsPerFrame
              val xm1   = x1 + (x2 - x1) * xw1
              val xm2   = x1 + (x2 - x1) * xw2
              gp.moveTo(xm1, y1)
              gp.lineTo(x1, yin1)
              gp.lineTo(x1, yout1)
              gp.lineTo(xm2, y2)
              gp.lineTo(x2, yout2)
              gp.lineTo(x2, yin2)
              gp.closePath()
              gp

            } else {
              rect.setRect(x1, y1, x2 - x1, y2 - y1)
              rect
            }
            g.setColor(Color.black)
            if (fill) g.fill(shp) else g.draw(shp)
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

            case Light =>
              bang(x01, x02, xw1 = if (fadeIn._1 == 0L) 0.0 else 1.0, xw2 = if (fadeOut._1 == 0L) 0.0 else 1.0)

            case Inject(ch1, src, _, recOpt) =>
              val ch2 = ch1 * 2 + (if (src) 0 else 1)
              val x11 = x01 + ch2 * pixelsPerChan + 1
              val x12 = x11 + pixelsPerChan - 2
              bang(x11, x12, fill = !src)

              if (drawRecordConn) recOpt.foreach { rec =>
                val ch3 = rec.chan * 2
                val x21 = x01 + ch3 * pixelsPerChan + 1
                val x22 = x21 + pixelsPerChan - 2
                val y3  = rec.span.stop * pixelsPerFrame
                line.setLine((x11 + x12) * 0.5, y1, (x21 + x22) * 0.5, y3)
                g.draw(line)
              }

            case _ =>
              bang(x01, x02)
          }

        case _ =>
      }

      xoff += w + procSpacing
    }
  }

  def readScore(f: File): Data = {
    val lines       = io.Source.fromFile(f, "UTF-8").getLines().filter(_.startsWith("<ANA>"))

    var res         = Vector.fill(procNum   )(Vector.empty[Region])
    var plateRec    = Vector.fill(NUM_PLATES)(Option.empty[Record])
    var injectPool  = Vector.fill(NUM_PLATES)(Map   .empty[String, Record])

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
          res = res.updated(idx, res(idx) :+ Region(Span(frame, frame + dur), detail = Wind(ch1, ch2)))

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
          val idx   = procID(name)
          val init :+ (last: Region) = res(idx)
          if (name == "licht") {
            require(words(5) == "width")
            val width = words(6).toInt
            val fdt     = 17.0 / (width + 18) // approx
            val isLTR   = (init.size % 2) == 0
            val fdfr    = (fdt * dur + 0.5).toLong
            val fadeIn  = if (isLTR) (0L, fdfr) else (fdfr, 0L)
            val fadeOut = fadeIn.swap
            res = res.updated(idx, init :+ last.copy(fadeIn = fadeIn, fadeOut = fadeOut, detail = Light))
          } else {
            res = res.updated(idx, init :+ last.copy(fadeIn = (dur, dur)))
          }

        case "fade-in-channel" =>
          val chOff = words(3).toInt
          // val ch    = words(4).toInt
          val dur   = words(5).toLong
          val name  = words(6)
          val idx   = procID(name)
          val init :+ (last: Region) = res(idx)
          val oldFade = last.fadeIn
          val newFade = if (chOff == 0) oldFade.copy(_1 = dur) else oldFade.copy(_2 = dur)
          res = res.updated(idx, init :+ last.copy(fadeIn = newFade))

        case "fade-out" =>
          val dur   = words(3).toLong
          val name  = words(4)
          val idx   = procID(name)
          val init :+ (last: Region) = res(idx)
          res = res.updated(idx, init :+ last.copy(span = Span(last.span.start, frame + dur), fadeOut = (dur, dur)))

        case "fade-out-channel" =>
          val ch    = words(3).toInt
          if (ch == 0) {
            val dur   = words(4).toLong
            val name  = words(5)
            val idx   = procID(name)
            val init :+ (last: Region) = res(idx)
            res = res.updated(idx, init :+ last.copy(span = Span(last.span.start, frame + dur), fadeOut = (dur, dur)))
          }

        case s if s.startsWith("plate<") =>
          val j     = s.indexOf(">-", 6)
          val plate = s.substring(6, j).toInt
          val cmd   = s.substring(j + 2)

          def stopPlate(key: String, fdt: Long) {
            val idx = procID("plates")
            val seq = res(idx)
            val j   = seq.lastIndexWhere(_.detail match {
              case Inject(_, false, `key`, _) => true
              case _ => false
            })
            val Region(Span.HasStart(start), (fadeIn0, _), _, detail) = seq(j)
            val fadeIn = math.min(fadeIn0, frame - start)
            res = res.updated(idx, seq.updated(j, Region(Span(start, frame + fdt),
              fadeIn = (fadeIn, fadeIn), fadeOut = (fdt, fdt), detail = detail)))
          }

          cmd match {
            case "reset" =>
            case "balance" =>
            case "create" =>
              val key       = words(3)
              require(words(4) == "death-dur" && words(6) == "fade")
              val dur       = words(5).toLong
              val fdt       = words(7).toLong
              val isInject  = key.startsWith("inject")
              val rec       = if (isInject) Some(injectPool(plate).getOrElse(key, sys.error(s"At $frame plate $plate key $key"))) else None
              val in        = Inject(plate, source = false, name = key, rec = rec)
              val idx       = procID("plates")
              res = res.updated(idx, res(idx) :+ Region(Span.from(frame), fadeIn = (fdt, fdt), detail = in))

            case "stop" =>
              val key   = words(3)
              require(words(4) == "fade" && words(6) == "shade")
              val fdt   = words(5).toLong
              val shade = words(7).toBoolean
              if (!shade) {
                stopPlate(key, fdt)
              }

            case "release" =>
              val key   = words(3)
              require(words(4) == "fade")
              val fdt   = words(5).toLong
              stopPlate(key, fdt)

            case "produce" =>
              require(words(3) == "dur")
              val dur = words(4).toLong
              plateRec = plateRec.updated(plate, Some(Record(plate, Span(frame, frame + dur))))

            case "rec-done" =>
              require(words(3) == "neighbour" && words(5) == "transform")
              val neigh = words(4).toInt
              val trans = words(6).toInt

            case "inject" =>
              require(words(3) == "neighbour" && words(5) == "transform" && words(7) == "path")
              val neigh = words(4).toInt
              val trans = words(6).toInt
              val key   = s"${words(8)}_$neigh"
              val rec   = plateRec(plate).get
              val old   = injectPool(neigh)
              injectPool = injectPool.updated(neigh, old + (key -> rec))
              val idx       = procID("plates")
              res = res.updated(idx, res(idx) :+ Region(rec.span, detail = Inject(plate, source = true, name = key, rec = None)))
          }

        case other =>
          // println(s"Warning: skipping command '$other'")
      }
    }

    res
  }
}