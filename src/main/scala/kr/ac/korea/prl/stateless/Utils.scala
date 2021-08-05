package kr.ac.korea.prl.stateless.Utils

import org.jgrapht.graph.DefaultEdge
import org.jgrapht.graph.DirectedAcyclicGraph

import scala.collection.JavaConverters._
import scala.util.{Try, Success, Failure}

object Utils {
  case class EmptyInputList(msg: String) extends Exception

  def catMaybes[A](optionList: List[Option[A]]): List[A] = for {
    some <- optionList.filter(opt => !opt.isEmpty)
    value <- some
  } yield value

  def getParents[A](
      from: DirectedAcyclicGraph[A, DefaultEdge],
      ofThis: A
  ): List[A] =
    from.incomingEdgesOf(ofThis).asScala.toList.map(from.getEdgeSource(_))

  /** Find the key with maximum value in a list of tuples (alist).
    *
    * @param alist: Tuple list where the second element is an integer.
    * @return the max key with the maximum value.
    */
  def findKeysWithMaxVal[A](
      alist: List[(A, Int)]
  ): List[A] = {
    if (alist.isEmpty)
      throw EmptyInputList("")
    val vals = alist.map(_._2)
    val maxVal = vals.max
    alist.filter(tup => tup._2 == maxVal).map(_._1)
  }
}
