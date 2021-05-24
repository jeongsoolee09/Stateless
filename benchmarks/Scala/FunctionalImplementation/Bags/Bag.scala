import scala.util.chaining._

trait Node[+Element]
case class NilNode() extends Node[Nothing]
case class NodeClass[Element](content: Element,
                              nextElement: Node[Element]) extends Node[Element]

class Bag[Element] {
  def add(stateTup: (Node[Element], Int), element: Element):
      (Node[Element], Int) = {
    val (firstElement, size) = stateTup
    val firstElement1 = new NodeClass[Element](element, firstElement)
    (firstElement1, size+1)
  }

  def nodeToString(node: Node[Element]): String = node match {
    case NilNode() => "nil"
    case NodeClass(value, node) => s"${value} -> ${nodeToString(node)}"
  }

  def stateToString(stateTup: (Node[Element], Int)) = {
    val (firstElement, size) = stateTup
    s"node: ${nodeToString(firstElement)}, size: ${size}"
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val bag: Bag[String] = new Bag()

    (NilNode(), 0).pipe(s => bag.add(s, "1"))
      .tap(s => println(bag.stateToString(s)))
      .pipe(s => bag.add(s, "2"))
      .tap(s => println(bag.stateToString(s)))
      .pipe(s => bag.add(s, "3"))
      .tap(s => println(bag.stateToString(s)))
      .pipe(s => bag.add(s, "4"))
      .tap(s => println(bag.stateToString(s)))
  }
}
