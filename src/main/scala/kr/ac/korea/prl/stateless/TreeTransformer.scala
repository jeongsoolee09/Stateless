package kr.ac.korea.prl.stateless.TreeTransformer

import scala.annotation.tailrec
import scala.meta._

import kr.ac.korea.prl.stateless.TreeTraverser._
import kr.ac.korea.prl.stateless.CustomTree._
import kr.ac.korea.prl.stateless.TreeGraph._

import scala.collection.JavaConverters._
import kr.ac.korea.prl.stateless.CustomTreeTranslator.CustomTreeTranslator
import org.jgrapht.traverse.BreadthFirstIterator
import org.jgrapht.graph.DirectedAcyclicGraph
import org.jgrapht.graph.DefaultEdge

case object TODO extends Exception

class TreeTransformer {

  def truncatedString(tree: CustomTree): String = {
    val string = tree.toString()
    string.slice(0, 15)+"..."
  }


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


  /**
    *
    *
    * @param defun
    * @param to
    */
  def signatureTransformer(defun: CustomTree,
                           to: List[(TermName, TypeName)]): Unit = {
    if (!defun.isInstanceOf[DefDef]) {
      throw new InvalidInput(truncatedString(defun))
    }
    val graph = TreeGraph.treeGraphOfCustomTree(defun)

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


  def addImport(toplevel: CustomTree,
                packageString: String): CustomTree =
    toplevel match {
      case TermBlock(stats) => {
        val importStat = ("import "+packageString).parse[Stat].get.asInstanceOf[Import]
        val customImportStat = CustomTreeTranslator.customTreeOfScalaMeta(importStat).asInstanceOf[CustomStat]
        TermBlock(customImportStat::stats)
      }
      case _ => toplevel
    }


  /**
    *
    *
    * @param defun
    */
  def pipingAdder(defclass: CustomTree, defun: CustomTree): CustomTree = {
    if (!defun.isInstanceOf[DefDef]) {
      throw new InvalidInput(truncatedString(defun))
    }

    /* adding import statement */
    val body = defun.asInstanceOf[DefDef].body
    val newbody = body match {
      case current @ TermBlock(stats) =>
        addImport(defun, "scala.util.chaining._").asInstanceOf[CustomTerm]
      case _ =>
        println("""TreeTransformer.pipingAdder:
No changes made since function body is a single statement."""); body
    }

    val toplevelVars = toplevelVarSpotter(defclass)
      ???
  }


  def replaceVertices(graph: DirectedAcyclicGraph[CustomTree, DefaultEdge],
                      from: CustomTree,
                      to: CustomTree) = {
    graph.addVertex(to)

    val incomingEdges = graph.incomingEdgesOf(from).asScala
    val outgoingEdges = graph.outgoingEdgesOf(from).asScala

    val parents = incomingEdges.map(graph.getEdgeSource(_))
    val children = outgoingEdges.map(graph.getEdgeTarget(_))

    parents.foreach(graph.removeEdge(_, from))
    children.foreach(graph.removeEdge(from, _))

    graph.removeVertex(from)

    parents.foreach(graph.addEdge(_, to))
    children.foreach(graph.addEdge(to, _))
  }


  def makeStubMain(customTree: CustomSource): CustomSource = {
    /*  */
    val treeGraph = TreeGraph.treeGraphOfCustomTree(customTree)
    val iterator = new BreadthFirstIterator(treeGraph)

    var defObjectBody = List[CustomStat]()
    var customTemplate = CustomTemplate(Nil, Nil, CustomSelf(CustomName(""), None), Nil)

    while (iterator.hasNext()) {
      val elem = iterator.next
      if (elem.isInstanceOf[DefObject] &&
            (elem.asInstanceOf[DefObject].name equals TermName("Main"))) {
        customTemplate = elem.asInstanceOf[DefObject].templ
        defObjectBody = customTemplate.stats
      }
    }

    // stubMain with newDefObjectBody => newCustomTemplate => newCustomSource (which is returned)

    val stubMain = DefDef(Nil, TermName("main"),
                          Nil, Nil, None, TermBlock(defObjectBody))
    val newCustomTemplate = CustomTemplate(customTemplate.early,
                                           customTemplate.inits,
                                           customTemplate.self,
                                           List(stubMain))

    ???
  }
}
