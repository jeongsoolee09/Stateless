package DataStructures.Graphs

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

class AdjacencyListGraph[E <: Comparable[E]] {
  
  // it fails to compile if it's private rather than protected...why?
  protected class Vertex(var data: E) {
    var adjacentVerticies: ListBuffer[Vertex] = ListBuffer()

    def addAdjacentVertex(to: Vertex): Boolean = {
      for (v: Vertex <- adjacentVerticies) {
        if (v.data.compareTo(to.data) == 0) {
          return false
        }
      }
      adjacentVerticies += to
      return true
    }

    def removeAdjacentVertex(to: E): Boolean = {
      for (i: Int <- 0 to adjacentVerticies.size-1) {
        if (adjacentVerticies(i).data.compareTo(to) == 0) {
          adjacentVerticies.remove(i)
          return true
        }
      }
      return false
    }
  }
  
  val verticies: ListBuffer[Vertex] = ListBuffer()

  def removeEdge(from: E, to: E): Boolean = {
    var fromV: Vertex = null
    breakable {
      for (v: Vertex <- verticies) {
        if (from.compareTo(v.data) == 0) {
          fromV = v
          break
        }
      }
    }
    if (fromV == null) return false
    fromV.removeAdjacentVertex(to)
  return true
  }

  def addEdge(from: E, to: E): Boolean = {
    var fromV: Vertex = null
    var toV: Vertex = null
    breakable {
      for (v: Vertex <- verticies) {
        if (from.compareTo(v.data) == 0) {
          fromV = v
        } else if (to.compareTo(v.data) == 0) {
          toV = v
        }
        if (fromV != null && toV != null) break
      }
    }
    if (fromV == null) {
      fromV = new Vertex(from)
      verticies += fromV
    }
    if (toV == null) {
      toV = new Vertex(to)
      verticies += toV
    }
    return fromV.addAdjacentVertex(toV)
  }

  override def toString(): String = {
    val sb: StringBuilder = new StringBuilder()
    for (v: Vertex <- verticies) {
      sb.append("Vertex: ")
      sb.append(v.data)
      sb.append("\n")
      sb.append("Adjacent verticies: ")
      for (v2: Vertex <- v.adjacentVerticies) {
        sb.append(v2.data)
        sb.append(" ")
      }
      sb.append("\n")
    }
    return sb.toString()
  }

  object Main extends App { 
    val graph: AdjacencyListGraph[Integer] =  new AdjacencyListGraph()
    assert(graph.addEdge(1, 2))
    assert(graph.addEdge(1, 5))
    assert(graph.addEdge(2, 5))
    assert(!graph.addEdge(1, 2))
    assert(graph.addEdge(2, 3))
    assert(graph.addEdge(3, 4))
    assert(graph.addEdge(4, 1))
    assert(!graph.addEdge(2, 3))
    println(graph)
  }
}
