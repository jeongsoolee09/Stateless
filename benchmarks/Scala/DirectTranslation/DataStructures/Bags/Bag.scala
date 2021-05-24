package DataStructures.Bags

import java.util.Iterator
import java.util.NoSuchElementException
import java.lang.Iterable
import java.util.function.Consumer

class Bag[Element] extends Iterable[Element] {

  private class Node[Element] {
    var content: Element = _
    var nextElement: Node[Element] = _
  }

  private class ListIterator[Element](var firstElement: Node[Element])
  extends Iterator[Element] {
    private var currentElement: Node[Element] = firstElement

    def hasNext(): Boolean = currentElement != null

    override def remove(): Unit = throw new UnsupportedOperationException()

    def next(): Element = {
      if (!hasNext()) throw new NoSuchElementException()
      val element = currentElement.content
    currentElement = currentElement.nextElement
  element
    }
  }

  private var firstElement: Node[Element] = null
  private var size: Int = 0

  def isEmpty(): Boolean = firstElement == null

  def getSize(): Int = size

  def add(element: Element): Unit = {
    val oldfirst: Node[Element] = firstElement
    firstElement = new Node()
    firstElement.content = element
    firstElement.nextElement = oldfirst
    size += 1
  }

  def iterator(): Iterator[Element] = new ListIterator(firstElement)

  def contains(element: Element): Boolean = {
    val iterator_ = iterator()
    while (iterator.hasNext()) {
      if (iterator.next() == element) {
        return true
      }
    }
    return false
  }

}

object Main extends App {
  val bag: Bag[String] = new Bag()

  bag.add("1")
  bag.add("1")
  bag.add("2")

  println("size of bag = " + bag.getSize())
  bag.forEach((s:String) => println(s))

  println(bag.contains(null))
  println(bag.contains("1"))
  println(bag.contains("3"))
}
