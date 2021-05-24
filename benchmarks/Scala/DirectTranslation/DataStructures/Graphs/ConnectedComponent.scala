package DataStructures.Graphs

import java.util.HashSet
import java.util.Set
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

class Graph[E <: Comparable[E]] {
  class Node(var name: E) { }

  class Edge(var startNode: Node, var endNode: Node) { }

  var edgeList: ListBuffer[Edge] = ListBuffer()
  var nodeList: ListBuffer[Node] = ListBuffer()

  def addEdge(startNode: E, endNode: E): Unit = {
    var start: Node = null; var end: Node = null
    for (node <- nodeList) {
      if (startNode.compareTo(node.name) == 0)
        start = node
      else if (endNode.compareTo(node.name) == 0)
        end = node
    }

    if (start == null) {
      start = new Node(startNode)
      nodeList += start
    }
    if (end == null) {
      start = new Node(endNode)
      nodeList += end
    }

    edgeList += new Edge(start, end)
  }

  def depthFirstSearch(n: Node, visited: ListBuffer[Node]): ListBuffer[Node] = {
    visited += n
    for (e: Edge <- edgeList) {
      if (e.startNode.equals(n) && !visited.contains(e.endNode)) {
        depthFirstSearch(e.endNode, visited)
      }
    }
    visited
  }

  def countGraphs(): Int = {
    var count: Int = 0
    val markedNodes: Set[Node] = new HashSet[Node]()

    for (n: Node <- nodeList) {
      if (!markedNodes.contains(n)) {
        markedNodes.add(n)
        markedNodes.addAll(depthFirstSearch(n, new ListBuffer[Node]).asJava)
        count += 1
      }
    }

    count
  }
}

object Main extends App {
  val graphChars: Graph[Character] = new Graph()

  graphChars.addEdge('a', 'b')
  graphChars.addEdge('a', 'e')
  graphChars.addEdge('b', 'e')
  graphChars.addEdge('b', 'c')
  graphChars.addEdge('c', 'd')
  graphChars.addEdge('d', 'a')

  graphChars.addEdge('x', 'y')
  graphChars.addEdge('x', 'z')

  graphChars.addEdge('w', 'w')

  val graphInts: Graph[Integer] = new Graph()

  graphInts.addEdge(1, 2)
  graphInts.addEdge(2, 3)
  graphInts.addEdge(2, 4)
  graphInts.addEdge(3, 5)

  graphInts.addEdge(7, 8)
  graphInts.addEdge(8, 10)
  graphInts.addEdge(10, 8)

  println("Amount of different char-graphs: " + graphChars.countGraphs())
  println("Amount of different int-graphs: " + graphInts.countGraphs())
}
