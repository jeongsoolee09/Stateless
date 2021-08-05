/** Constructing callgraph in a very YAME manner. */

import org.jgrapht.Graph._
import org.jgrapht.graph.DirectedAcyclicGraph
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.Graphs._
import org.jgrapht.traverse.BreadthFirstIterator

import kr.ac.korea.prl.stateless.CustomTree._
import kr.ac.korea.prl.stateless.CustomTree.Predicates._
import kr.ac.korea.prl.stateless.TreeTraverser._
import kr.ac.korea.prl.stateless.TreeGraph._
import kr.ac.korea.prl.stateless.Utils.Utils._

import scala.collection.mutable.ListBuffer
import scala.util.{Try, Success, Failure}

object CallGraph {

  type Method = (TypeName, TermName)
  type CallGraph = DirectedAcyclicGraph[Method, DefaultEdge]
  type BFS = BreadthFirstIterator[CustomTree, DefaultEdge]

  case class MethResolutionFailed(msg: String) extends Exception

  def makeStubMain(): Method = (TypeName("ph"), TermName("main"))

  def callGraphConstructor(ctree: CustomTree): CallGraph = {
    val x = new CallGraph(classOf[DefaultEdge])
    ???
  }

  /** Resolve the method's signature only by its name.
    *
    * Limitations:
    * - Does not support overloaded methods:
    *   It cannot specify which definition among others.
    *
    * @param funName
    * @param method
    * @return
    */
  def methodNameResolver(target: TermName, namespace: CustomSource): Method = {
    val treeGraph = TreeGraph.graphFromCustomTree(namespace)
    val iterator = new BFS(treeGraph)

    var defunFound = false
    val parentClassDefs = new ListBuffer[CustomTree]
    while (iterator.hasNext) {
      val elem = iterator.next()
      if (
        elem.isInstanceOf[DefDef] &&
        isDefun(elem)
      ) {
        var defunFound = true
        val defun = elem.asInstanceOf[DefDef]
        if (defun.name equals target) {
          getParents(treeGraph, elem)
            .filter((parent: CustomTree) =>
              isDefClass(parent) || isDefObject(parent)
            )
            .foreach(parentClassDefs += _)
        }
      }
    }

    if (defunFound) {
      /* ============ The value was found in the current namespace ============ */
      // get the most specific one!
      val classesAndDepth =
        parentClassDefs.map((klass => (klass, iterator.getDepth(klass)))).toList
      try {
        val result = findKeysWithMaxVal[CustomTree](classesAndDepth)
        (result.head.asInstanceOf[TypeName], target)
      } catch {
        case _: EmptyInputList =>
          throw EmptyInputList(s"elem: ${target.toString}")
      }
    } else {
      /* ============ The value was not found in the current namespace ============ */
      // check the import statements
      ???
    }
  }
}
