package DataStructures.Graphs

import java.util._
import scala.util.control.Breaks._

class BellmanFord(var v: Int, var e: Int) {
  private var vertex = v
  private var edge = e
  private var index: Int = 0
  private var edges = new Array[Edge](e)

  class Edge(var u: Int, var v: Int, var w: Int) { }

  def printPath(p: Array[Int], i: Int): Unit = {
    if (p(i) == -1)
      return
    printPath(p, p(i))
    print(i + " ")
  }

  def go(): Unit = {
    val sc: Scanner = new Scanner(System.in)
    var i, v, e, u, ve, w, j, neg = 0
    println("Enter no. of vertices and edges please")
    v = sc.nextInt()
    e = sc.nextInt()
    val arr: Array[Edge] = new Array(e)
    println("Input edges")
    for (i <- 0 to e-1) {
      u = sc.nextInt()
      ve = sc.nextInt()
      w = sc.nextInt()
      arr(i) = new Edge(u, ve, w)
    }
    val dist: Array[Int] = new Array(v)
    val p: Array[Int] = new Array(v)
    for (i <- 0 to v-1)
      dist(i) = Integer.MAX_VALUE
    dist(0) = 0
    p(0) = -1
    for (i <- 0 to v-2) {
      for (j <- 0 to e-1) {
        if (dist(arr(j).u).asInstanceOf[Int] == Integer.MAX_VALUE
                  && dist(arr(j).v) > dist(arr(j).u) + arr(j).w) {
              dist(arr(j).v) = dist(arr(j).u) + arr(j).w
              p(arr(j).v) = arr(j).u
        }
      }
    }
    breakable {
    for (j <- 0 to e-1) {
      if (dist(arr(j).u).asInstanceOf[Int] != Integer.MAX_VALUE && dist(arr(j).v) > dist(arr(j).u) + arr(j).w) {
        neg = 1
        println("Negative cycle")
        break
      }
    } }
    if (neg == 0) {
      println("Distances are: ")
      for (i <- 0 to v-1)
        println(i + " " + dist(i))
      println("Path followed:")
      for (i <- 0 to v-1) {
        print("0 ")
        printPath(p, i)
        println
      }
    }
    sc.close()
  }

  def show(
    source: Int,
    end: Int,
    arr: Array[Edge]
  ): Unit = {
    var i, j, v = vertex; var e = edge; var neg = 0
    val dist: Array[Double] = new Array(v)
    val p: Array[Int] = new Array(v)
    for (i <- 0 to v-1)
      dist(i) = Integer.MAX_VALUE
    dist(source) = 0
    p(source) = -1
    for (i <- 0 to v-2) {
      for (j <- 0 to e-1) {
        if (dist(arr(j).u).asInstanceOf[Int] != Integer.MAX_VALUE
              && dist(arr(j).v) > dist(arr(j).u) + arr(j).w) {
          dist(arr(j).v) = dist(arr(j).u) + arr(j).w
          p(arr(j).v) = arr(j).u
        }
      }
    }
    breakable {
    for (j <- 0 to e-1) {
      if (dist(arr(j).u) != Integer.MAX_VALUE && dist(arr(j).v) > dist(arr(j).u) + arr(j).w) {
        neg = 1
        println("Negative cycle")
        break
      }
    } }
    if (neg == 0) {
      println("Distance is: " + dist(end))
      println("Path followed:")
      print(source + " ")
      printPath(p, end)
      println()
    }
  }

  def addEdge(x: Int, y: Int, z: Int): Unit = {
    index += 1
    edges(index) = new Edge(x, y, z)
  }

  def getEdgeArray(): Array[Edge] = edges
}


object Main extends App {
  val obj: BellmanFord = new BellmanFord(0, 0)
  obj.go()
}
