//1
import java.awt
import java.beans.{
  PropertyChangeEvent,
  PropertyChangeListener,
  PropertyChangeSupport
}
import java.io.InputStream

import scala.annotation.tailrec
import scala.language.postfixOps

trait RectangleLike {
  rect: java.awt.geom.RectangularShape =>

  def translate(dx: Int, dy: Int) = {
    rect.setFrame(
      rect.getX + dx,
      rect.getY + dy,
      rect.getWidth,
      rect.getHeight
    )
    rect
  }

  def grow(dw: Int, dh: Int) = {
    rect.setFrame(
      rect.getX,
      rect.getY,
      rect.getWidth + dw,
      rect.getHeight + dh
    )
    rect
  }

}

val egg = new java.awt.geom.Ellipse2D.Double(5, 10, 20, 30) with RectangleLike
egg.translate(10, -10)
egg.grow(10, 20)
egg.getFrame

// 2
class OrderedPoint
    extends java.awt.Point
    with scala.math.Ordered[java.awt.Point] {
  def compare(that: awt.Point): Int = {
    if (x == that.x && y == that.y) 0
    else if (x > that.x || (x == that.x && y > that.y)) 1
    else -1
  }
}

val point1 = new OrderedPoint
val point2 = new OrderedPoint
val point3 = new OrderedPoint
val point4 = new OrderedPoint
point1.setLocation(0, 0)
point2.setLocation(1, 3)
point3.setLocation(1, 2)
point4.setLocation(1, 3)
point1 compare point2
point1 compare point3
point1 compare point4
point2 compare point3
point2 compare point4
point3 compare point4
point4 compare point1
point4 compare point2
point4 compare point3

// 4
trait CryptoLogger extends Logger {
  val key = 3
  abstract override def log(msg: String): Unit = {
    super.log(msg.map(c => (c + key).toChar))
  }
}

val logger1 = new ConsoleLogger with CryptoLogger
logger1.log("rush a")
val logger2 = new ConsoleLogger with CryptoLogger { override val key = -3 }
logger2.log("rush b")
logger1.log("orpe\u001D_")
logger2.log("uxvk#d")

// 5
trait UltimatePropertyChangeSupport {
  private val sup = new PropertyChangeSupport(this)

  def addPropertyChangeListener(listener: PropertyChangeListener): Unit =
    sup.addPropertyChangeListener(listener)

  def removePropertyChangeListener(listener: PropertyChangeListener): Unit =
    sup.removePropertyChangeListener(listener)

  def getPropertyChangeListeners = sup.getPropertyChangeListeners

  def addPropertyChangeListener(propertyName: String,
                                listener: PropertyChangeListener): Unit =
    sup.addPropertyChangeListener(propertyName, listener)

  def removePropertyChangeListener(propertyName: String,
                                   listener: PropertyChangeListener): Unit =
    sup.removePropertyChangeListener(propertyName, listener)

  def getPropertyChangeListeners(propertyName: String) =
    sup.getPropertyChangeListeners(propertyName)

  def firePropertyChange(propertyName: String,
                         oldValue: Any,
                         newValue: Any): Unit =
    sup.firePropertyChange(propertyName, oldValue, newValue)

  def firePropertyChange(propertyName: String,
                         oldValue: Int,
                         newValue: Int): Unit =
    sup.firePropertyChange(propertyName, oldValue, newValue)

  def firePropertyChange(propertyName: String,
                         oldValue: Boolean,
                         newValue: Boolean): Unit =
    sup.firePropertyChange(propertyName, oldValue, newValue)

  def firePropertyChange(event: PropertyChangeEvent): Unit =
    sup.firePropertyChange(event)

  def fireIndexedPropertyChange(propertyName: String,
                                index: Int,
                                oldValue: Any,
                                newValue: Any): Unit =
    sup.fireIndexedPropertyChange(propertyName, index, oldValue, newValue)

  def fireIndexedPropertyChange(propertyName: String,
                                index: Int,
                                oldValue: Int,
                                newValue: Int): Unit =
    sup.fireIndexedPropertyChange(propertyName, index, oldValue, newValue)

  def fireIndexedPropertyChange(propertyName: String,
                                index: Int,
                                oldValue: Boolean,
                                newValue: Boolean): Unit =
    sup.fireIndexedPropertyChange(propertyName, index, oldValue, newValue)

  def hasListeners(propertyName: String) =
    sup.hasListeners(propertyName)
}

class PointBean(x: Int = 0, y: Int = 0)
    extends java.awt.Point(x, y)
    with UltimatePropertyChangeSupport

val bean = new PointBean(1, 2)
bean.addPropertyChangeListener("bestListener", (evt: PropertyChangeEvent) => {
  println(evt)
})
bean.getPropertyChangeListeners(0)

// 8
trait BufferedInputStreamLike extends InputStream with Logger {
  val bufferSize = 2048
  private val buffer = new Array[Byte](bufferSize)
  private var offset: Int = 0
  private var size: Int = 0

  abstract override def read(): Int = {
    if (size == -1) {
      return -1
    }

    if (offset >= size) {
      offset = 0
      size = 0

      log("Filling the buffer, bufferSize: " + bufferSize)
      fillBuffer(0)

      if (size == 0) {
        log("Reached stream end")
        size = -1
        return -1
      }

      log("Buffer is filled, size: " + size)
    }

    val byte = buffer(offset)
    offset += 1
    byte
  }

  @tailrec
  private def fillBuffer(index: Int): Unit = {
    if (index >= buffer.length) {
      return
    }

    val byte = super.read()
    if (byte == -1) {
      return
    }

    buffer(index) = byte.toByte
    size += 1
    fillBuffer(index + 1)
  }
}

// 10
trait ConsoleLogger extends Logger {
  override def log(msg: String): Unit = println(msg)
}
