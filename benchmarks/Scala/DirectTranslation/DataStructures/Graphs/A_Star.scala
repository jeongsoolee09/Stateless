package A_Star

import scala.collection.mutable.ListBuffer
import java.util._

class A_Star {
  private class Edge(private var from: Int,
                     private var to: Int,
                     private var weight: Int) {

    def getFrom(): Int = from

    def getTo(): Int = to

    def getWeight(): Int = weight
  }

  private class Graph(var size: Int) {
    graph = new ListBuffer(size)
    for (i: Int <- 0 to size-1)
      this.graph(i) = new ArrayList()

    private def getNeighbours(from: Int) = this.graph(from)

    private def addEdge(edge: Edge): Unit = {
      this.graph(edge.getFrom()).add(new Edge(edge.getFrom(), edge.getTo(), edge.getWeight()))
      this.graph(edge.getTo()).add(new Edge(edge.getTo(), edge.getFrom(), edge.getWeight()))
    }
  }

  private class PathAndDistance(private var distance: int,
                                private var path: ArrayList[Integer],
                                private var estimated: Int) {

    def getDistance(): Int = distance

    def getPath() = path

    def getEstimated() = estimated

    def printSolution: Unit = {
      if (this.path != null)
        println("Optimal path: " + this.path.toString() + ", distance: " + this.distance)
      else
        println("There is no path available to connect the points")
    }

    private def initializeGraph(graph: Graph, data: ArrayList[Integer]) = {
      for (i: Int <- Range(0, data.size(), 4))
        graph.addEdge(new Edge(data.get(i), data.get(i+1), data.get(i+2)))
    }
  }

  def aStar(from: Int, to: Int, graph: Graph, heuristic: Array[Int]): PathAndDistance = {
    val queue: PriorityQueue[PathAndDistance] =
      new PriorityQueue(Comparator.comparingInt(a => (a.getDistance()+a.getEstimated())))

    queue.add(new PathAndDistance(0, new ArrayList(Arrays.asList(from)), 0))

    var solutionFound = false
    var currentData: PathAndDistance = new PathAndDistance(-1, null, -1)
    while (!queue.isEmpty() && !solutionFound) {
      currentData = queue.poll()
      val currentPosition: Int = currentData.getPath().get(currentData.getPath.size() - 1)
      if (currentPotision == to) solutionFound = true
      else
        for (edge: Edge <- graph.getNeighbours(currentPosition))
          if (!currentData.getPath().contains(edge.getTo())) {
            val updatedPath: ArrayList[Integer] = new ArrayList(currentData.getPath())
            updatedPath.add(edge.getTo())
            queue.add(
              new PathAndDistance(
                currentData.getDistance() + edge.getWeight(),
                updatedPath,
                heuristic(edge.getTo())))
          }
    }
    if (solutionFound) currentData else new PathAndDistance(-1, null, -1)
  }
}

object Main extends App {
  def main: Unit = {
    var heuristic: Array[Int] = Array(366, 0, 160, 242, 161, 178, 77, 151, 226, 244, 241, 234, 380, 98, 193, 253, 329, 80, 199, 374)
    var graph = new Graph(20)
    var graphData: ArrayList =
      new ArrayList(
        Arrays.asList(0, 19, 75, null, 0, 15, 140, null, 0, 16, 118, null, 19, 12, 71, null, 12, 15, 151,
                      null, 16, 9, 111, null, 9, 10, 70, null, 10, 3, 75, null, 3, 2, 120, null, 2, 14,
                      146, null, 2, 13, 138, null, 2, 6, 115, null, 15, 14, 80, null, 15, 5, 99, null, 14,
                      13, 97, null, 5, 1, 211, null, 13, 1, 101, null, 6, 1, 160, null, 1, 17, 85, null,
                      17, 7, 98, null, 7, 4, 86, null, 17, 18, 142, null, 18, 8, 92, null, 8, 11, 87))
    initializeGraph(graph, graphData)

    var solution: PathAndDistance = aStar(3, 1, graph, heuristic)
    solution.printSolution()
  }
}
