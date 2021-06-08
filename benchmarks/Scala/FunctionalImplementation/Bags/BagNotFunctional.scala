class Bag[Element] {
  private class Node[Element] {
    var content: Element = _
    var nextElement: Node[Element] = _
  }

  private var firstElement: Node[Element] = null
  private var size: Int = 0

  def add(element: Element): Unit = {
    val oldfirst: Node[Element] = firstElement
    firstElement = new Node()
    firstElement.content = element
    firstElement.nextElement = oldfirst
    size += 1
  }

  def nodeToString(node: Node[Element]) =
    if (node == null) "nil"
    else if (node.nextElement == null) s"${node.content} -> nil"
    else s"${node.content} -> ${node.nextElement.content}"

  def stateToString() =
    "node: "+nodeToString(firstElement).toString()+", size: "+size.toString()
}

object Main {
  def main(args: Array[String]): Unit = {
    val bag: Bag[String] = new Bag()

    bag.add("1")
    println(bag.stateToString())
    bag.add("2")
    println(bag.stateToString())
    bag.add("3")
    println(bag.stateToString())
    bag.add("4")
    println(bag.stateToString())
  }
}
