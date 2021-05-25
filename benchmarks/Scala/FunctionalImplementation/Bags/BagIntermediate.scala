import scala.util.chaining._

class Bag[Element] {

  protected class Node[Element] {
    var content: Element = _
    var nextElement: Node[Element] = _
  }

  def add(stateTup: (Node[Element], Int), element: Element):
      (Node[Element], Int) = {
    val (firstElement, size) = stateTup
    val oldfirst: Node[Element] = firstElement
    val firstElement1 = new Node[Element]()
    firstElement1.content = element
    firstElement1.nextElement = oldfirst
    (firstElement1, size+1)
  }

  def nodeToString(node: Node[Element]) =
    if (node == null) "nil"
    else if (node.nextElement == null) s"${node.content} -> nil"
    else s"${node.content} -> ${node.nextElement.content}"


  def stateToString(stateTup: (Node[Element], Int)) = {
    val (firstElement, size) = stateTup
    s"node: ${nodeToString(firstElement)}, size: ${size}"
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val bag: Bag[String] = new Bag()

    (null, 0).pipe(s => bag.add(s, "1"))
      .tap(s => println(bag.stateToString(s)))
      .pipe(s => bag.add(s, "2"))
      .tap(s => println(bag.stateToString(s)))
      .pipe(s => bag.add(s, "3"))
      .tap(s => println(bag.stateToString(s)))
      .pipe(s => bag.add(s, "4"))
      .tap(s => println(bag.stateToString(s)))
  }
}
