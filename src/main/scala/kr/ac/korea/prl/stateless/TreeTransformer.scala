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

    val incomingEdges = graph.incomingEdgesOf(fromTermParam).asScala
    val outgoingEdges = graph.outgoingEdgesOf(fromTermParam).asScala

    val parents = incomingEdges.map(graph.getEdgeSource(_))
    val children = outgoingEdges.map(graph.getEdgeTarget(_))

    parents.foreach(graph.removeEdge(_, fromTermParam))
    children.foreach(graph.removeEdge(fromTermParam, _))

    graph.removeVertex(fromTermParam)

    parents.foreach(graph.addEdge(_, toTermParam.asInstanceOf[CustomTree]))
    children.foreach(graph.addEdge(toTermParam.asInstanceOf[CustomTree], _))
  }
}
