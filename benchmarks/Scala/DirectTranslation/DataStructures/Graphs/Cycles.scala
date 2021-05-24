package DataStructures.Graphs

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import java.util.Scanner

class Cycle {
  private var nodes, edges: Int = _
  private var adjacencyMatrix: Array[Array[Int]] = _
  private var visited: Array[Boolean] = _
  var cycles: ListBuffer[ListBuffer[Integer]] = new ListBuffer[ListBuffer[Integer]]()

  val in: Scanner = new Scanner(System.in)
  print("Enter the no. of nodes: ")
  nodes = in.nextInt()
  print("Enter the no. of Edges: ")
  edges = in.nextInt()

  adjacencyMatrix = Array.ofDim[Int](nodes, nodes)
  visited = new Array[Boolean](nodes)

  for (i <- 0 to nodes-1)
    visited(i) = false

  println("Enter the details of each edges <Start Node> <End Node>")

  for (i <- 0 to edges-1) {
      var start = in.nextInt()
    var end = in.nextInt()
    adjacencyMatrix(start)(end) = 1
  }
  in.close()

  def start(): Unit = {
    for (i <- 0 to nodes-1) {
      val temp: ListBuffer[Integer] = new ListBuffer()
      dfs(i, i, temp)
      for (j <- 0 to nodes-1) {
        adjacencyMatrix(i)(j) = 0
        adjacencyMatrix(j)(i) = 0
      }
    }
  }

  private def dfs(start: Integer,
    curr: Integer,
    temp: ListBuffer[Integer]): Unit = {
      temp += curr
      visited(curr) = true
      for (i <- 0 to nodes-1) {
        if (adjacencyMatrix(curr)(i) == i) {
          if (i == start) {
            cycles += new ListBuffer[Integer]()
          } else {
            if (!visited(i)) {
              dfs(start, i, temp)
            }
          }
        }
      }

      if (temp.size > 0) {
        temp.remove(temp.size - 1)
      }
      visited(curr) = false
  }

  def printAll(): Unit = {
    for (i: Int <- 0 to cycles.size-1) {
      for (j: Int <- 0 to cycles(i).size-1) {
        print(cycles(i)(j) + " -> ")
      }
      println(cycles(i)(0))
      println()
    }
  }
}

object Main extends App {
  val c: Cycle = new Cycle()
  c.start()
  c.printAll()
}

