package kr.ac.korea.prl.stateless.TreeTransformer

import scala.annotation.tailrec
import scala.meta._

import kr.ac.korea.prl.stateless.TreeTraverser._
import kr.ac.korea.prl.stateless.CustomTree._
import kr.ac.korea.prl.stateless.TreeGraph._

import scala.collection.JavaConverters._

case object TODO extends Exception

class TreeTransformer {

  /**
    * Remove all top-level vars in the class definitions.
    *
    * @param tree
    * @return
    */
  def isToplevel(varTup: (TypeName, TermName, TermName,
                          Option[CustomType], Option[CustomTerm]),
                 tree: CustomTree) = {
    val innerClassInfo = TreeTraverser.innerClassChainCollector(tree)
    val toplevelClasses = innerClassInfo.foldLeft(List[TypeName]())((acc, tup) =>
      if (tup._1 equals TypeName("ph")) tup._1::acc else acc
    )
    toplevelClasses.foldLeft(false)((acc, classType) =>
      (classType equals varTup._1) || acc)
  }


  /**
    * Spot all toplevel vars in a tree.
    *
    * @param tree
    * @return tuples describing all toplevel vars
    */
  def toplevelVarSpotter(tree: CustomTree): List[(TypeName, TermName, TermName,
                                                  Option[CustomType], Option[CustomTerm])] =
    TreeTraverser.varCollector(tree).filter(isToplevel(_, tree))


  /**
    * Remove all toplevel vars in a tree.
    *
    * @param tree
    * @return the same tree with toplevel vars removed.
    */
  def toplevelVarRemover(tree: CustomTree): CustomTree = {
    val toplevelVars = toplevelVarSpotter(tree)
    tree match {
      case DefClass(mods, name, tparams, ctor, CustomTemplate(early, inits, self, stats)) => {
        val filtered = stats.filter(toplevelVars.contains(_))
        DefClass(mods, name, tparams, ctor, CustomTemplate(early, inits, self, filtered))
      }
      case CustomSource(stats) =>
        CustomSource(stats.filter(TreeTraverser.scopeGreaterThanClass)
                       .map(toplevelVarRemover(_).asInstanceOf[CustomStat]))

      case CustomPkg(ref, stats) =>
        CustomPkg(ref, stats.filter(TreeTraverser.scopeGreaterThanClass)
                    .map(toplevelVarRemover(_).asInstanceOf[CustomStat]))
    }
  }


  def signatureTransformer(defun: CustomTree,
                           to: List[(TermName, TypeName)]) = {
    val graph = TreeGraph.graphFromCustomTree(defun)

    def constructTermParam(material: List[(TermName, TypeName)]) = {
      val paramList = to.map(
        (tup: (TermName, TypeName)) => {
          val id = tup._1
          val tpe = tup._2
          List(Nil, id, Some(tpe), None)
        }
      )
      paramList
    }

    val fromTermParam = TreeTraverser.paramListRetreiver(defun)
    val toTermParam = constructTermParam(to)

    // reconstruct the edges
    for {
      edge <- graph.incomingEdgesOf(fromTermParam.asInstanceOf[CustomTree]).asScala
      val parent = graph.getEdgeSource(edge)
      val _ = graph.addVertex(parent)
      val _ = graph.addEdge(parent, toTermParam.asInstanceOf[CustomTree])
    }

    for {
      edge <- graph.outgoingEdgesOf(fromTermParam.asInstanceOf[CustomTree]).asScala
      val child = graph.getEdgeTarget(edge)
      val _ = graph.addVertex(child)
      val _ = graph.addEdge(child, toTermParam.asInstanceOf[CustomTree])
    }

    // remove the vertex
    graph.removeVertex(fromTermParam)

    graph
  }
}
