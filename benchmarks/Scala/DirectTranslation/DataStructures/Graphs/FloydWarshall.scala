package DataStructures.Graphs

import java.util.Scanner
import scala.util.control.Breaks._

class FloydWarshall(private var numberofvertices:Int) {
  private var DistanceMatrix: Array[Array[Int]] = Array.ofDim[Int](numberofvertices+1, numberofvertices+1)
  private val INFINITY: Int = 999

  def floydwarshall(AdjacencyMatrix: Array[Array[Int]]) = {
    for (source <- 1 to numberofvertices) {
      for (destination <- 1 to numberofvertices) {
        DistanceMatrix(source)(destination) = AdjacencyMatrix(source)(destination)
      }
    }
    for (intermediate <- 1 to numberofvertices) {
      for (source <- 1 to numberofvertices) {
        for (destination <- 1 to numberofvertices) {
          if (DistanceMatrix(source)(intermediate) + DistanceMatrix(intermediate)(destination) < DistanceMatrix(source)(destination))
          {
            DistanceMatrix(source)(destination) = DistanceMatrix(intermediate)(destination)
          }
        }
      }
    }
    for (source <- 1 to numberofvertices) print("\t" + source)
    println()
    for (source <- 1 to numberofvertices)  {
      print(source + "\t")
      for (destination <- 1 to numberofvertices) {
        print(DistanceMatrix(source)(destination) + "\t")
      }
      println()
    }
  }

  object Main extends App {
    val scan: Scanner = new Scanner(System.in)
    println("Enter the number of vertices")
    val numberOfVertices = scan.nextInt()
    val adjacencyMatrix = Array.ofDim[Int](numberOfVertices+1, numberOfVertices+1)
    println("Enter the Weighted Matrix for the graph")
    for (source <- 1 to numberOfVertices) {
      for (destination <- 1 to numberOfVertices) {
        breakable {
          adjacencyMatrix(source)(destination) = scan.nextInt()
          if (source == destination) {
            adjacencyMatrix(source)(destination) = 0
            break
          }
          if (adjacencyMatrix(source)(destination) == 0) {
            adjacencyMatrix(source)(destination) = INFINITY
          }
        }
      }
      println("The Transitive Closure of the Graph")
      val floydwarshall: FloydWarshall = new FloydWarshall(numberOfVertices)
      floydwarshall.floydwarshall(adjacencyMatrix)
      scan.close()
    }
  }
}

