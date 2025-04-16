import scala.collection.mutable

class EmptyBufferException() extends Exception {}

class FullBufferException() extends Exception {}

class CircularBuffer(var capacity: Int) extends mutable.ArrayDeque[Int](capacity) {

  def write(value: Int): Unit =
    ensureBufferIsNotFull()
    addOne(value)

  def read(): Int = {
    ensureBufferIsNotEmpty()
    removeHead()
  }

  def overwrite(value: Int) = ???

  private def ensureBufferIsNotFull(): Unit =
    if size == capacity then throw new FullBufferException

  private def ensureBufferIsNotEmpty(): Unit =
    if isEmpty then throw new EmptyBufferException
}