package DataStructures.DynamicArray

import java.util._
import java.util.function.Consumer
import java.util.stream.Stream
import java.util.stream.StreamSupport
import java.util.Arrays
import java.lang.Iterable

class DynamicArray[E](private var capacity: Int,
                      private var size: Int,
                      private var elements: Array[AnyRef]) extends Iterable[E] {

  def this(capacity: Int) = this(capacity, 0, new Array(capacity))

  def this() = this(10, 0, new Array(10))

  def newCapacity(): Int = {
    this.capacity *= this.capacity
    this.capacity
  }

  def add[E <: AnyRef](element: E): Unit = {
    if (size == elements.length)
      this.elements = Arrays.copyOf(elements, newCapacity())

    this.elements(this.size) = element
    size += 1
  }

  def put[E <: AnyRef](index: Int, element: E): Unit = this.elements(index) = element

  def get(index: Int): E = getElement(index)

  def remove(index: Int) = {
    val oldElement: E = getElement(index)
    fastRemove(this.elements, index)

    oldElement
  }

  def getSize(): Int = this.size

  def isEmpty(): Boolean = this.size == 0

  def stream(): Stream[E] = StreamSupport.stream(spliterator(), false)

  private def fastRemove(element: Array[Object], index: Int): Unit = {
    val newSize: Int = this.size - 1

    if (newSize > index)
      System.arraycopy(elements, index+1, elements, index, newSize-index)

    this.size = newSize
    element(this.size) = null
  }

  private def getElement(index: Int): E = this.elements(index).asInstanceOf[E]

  override def toString(): String = Arrays.toString(Arrays.stream(this.elements)
                                                      .filter(Objects.nonNull)
                                                      .toArray())

  private class DynamicArrayIterator extends Iterator[E] {
    private var cursor: Int = _

    override def hasNext(): Boolean = this.cursor != size

    override def next(): E = {
      if (this.cursor > DynamicArray.this.size)
        throw new NoSuchElementException()

      if (this.cursor > DynamicArray.this.elements.length)
        throw new ConcurrentModificationException()

      val element: E = DynamicArray.this.getElement(this.cursor)
      this.cursor += 1

      element
    }

    override def remove() {
      if (this.cursor < 0)
        throw new IllegalStateException()

      DynamicArray.this.remove(this.cursor)
      this.cursor -= 1
    }

    override def forEachRemaining(action: Consumer[_ >: E]) = {
      Objects.requireNonNull(action)

      for (i: Int <- 0 to DynamicArray.this.size)
        action.accept(DynamicArray.this.getElement(i))
    }
  }

  def iterator(): Iterator[E] = new DynamicArrayIterator()
}

object Main extends App {
  def main(): Unit = {
    var names: DynamicArray[String] = new DynamicArray()
    names.add("Peubes")
    names.add("Marley")

    names.forEach(println)

    names.stream().forEach(println)

    println(names)

    println(names.getSize())

    names.remove(0)

    names.forEach(println)
  }
}
