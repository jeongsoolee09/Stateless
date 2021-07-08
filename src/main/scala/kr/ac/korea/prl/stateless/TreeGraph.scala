/**
  * Scala source code <=> ScalaMeta Tree <=> CustomTree <=> TreeGraph <=> SummarizedTreeGraph
  *                                                     ^^^
  */

package kr.ac.korea.prl.stateless.TreeGraph

import org.jgrapht.graph.DirectedAcyclicGraph
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.traverse.BreadthFirstIterator
import org.jgrapht.Graphs._
import org.jgrapht.nio.dot.DOTExporter

import java.io.File

import scala.meta._
import scala.meta.contrib._
import scala.collection.JavaConverters._

import kr.ac.korea.prl.stateless.CustomTree._
import kr.ac.korea.prl.stateless.CustomTreeTranslator.CustomTreeTranslator

object TreeGraph {

  type TreeGraph = DirectedAcyclicGraph[CustomTree, DefaultEdge]
  type BFS = BreadthFirstIterator[CustomTree, DefaultEdge]


  def isDefun(tree: CustomTree): Boolean = tree match {
    case DefDef(_, _, _, paramList, _, _) => !paramList.isEmpty
    case _                                => false
  }


  def findRoot[A](graph: DirectedAcyclicGraph[A, DefaultEdge]): A = {
    val vertices = graph.vertexSet.asScala.toList
    val inDegrees = vertices.map(graph.inDegreeOf(_))
    val verticesAndDegrees = vertices.zip(inDegrees)
    val rootTuples = verticesAndDegrees.filter(tup =>
      tup._2 == 0
    )
    assert(rootTuples.length == 1) // since it's a tree
    rootTuples.head._1
  }


  def appendGraph[A](graph1: DirectedAcyclicGraph[A, DefaultEdge],
                     dockingPoint: A,
                     graph2: DirectedAcyclicGraph[A, DefaultEdge]): DirectedAcyclicGraph[A, DefaultEdge] = {
    addGraph(graph1, graph2)
    graph1.addEdge(dockingPoint, findRoot(graph2))
    graph1
  }


  /**
    * CustomTree to a DirectedAcyclicGraph.
    *
    * @param tree
    * @return
    */
  def graphFromCustomTree(tree: CustomTree): TreeGraph = {

    /** Do a preliminary move, i.e. registering current to the previous. */
    def preliminary(previous: CustomTree,
                    current: CustomTree,
                    acc: TreeGraph) = {
      acc.addVertex(current)
      if (!(previous equals current))
        acc.addEdge(previous, current)
    }

    /** Register a simple child to a current node. */
    def addSimpleNode(current: CustomTree,
                      child: CustomTree,
                      acc: TreeGraph) = {
      acc.addVertex(child.asInstanceOf[CustomTree])
      acc.addEdge(current, child.asInstanceOf[CustomTree])
    }

    /** Handle a List node by recursing on it. */
    def addListNode(previous: CustomTree,
                    listNode: List[CustomTree],
                    acc: TreeGraph) = {
      val stubListNode = ASTList()
      acc.addVertex(stubListNode)
      acc.addEdge(previous, stubListNode)
      listNode.foreach(inner(stubListNode, _, acc))
    }

    /** Handle a List of List of node by recursing on it. */
    def addOptionNode(previous: CustomTree,
                      optionNode: Option[CustomTree],
                      acc: TreeGraph) = optionNode match {

      case None => {
        // add the ASTNone stub node and finish
        val stubOptionNode = ASTNone()
        acc.addVertex(stubOptionNode)
        acc.addEdge(previous, stubOptionNode)
      }

      case Some(value) => {
        // first, add the ASTSome stub node
        val stubOptionNode = ASTSome()
        acc.addVertex(stubOptionNode)
        acc.addEdge(previous, stubOptionNode)

        // then, connect the content below the stub node
        acc.addVertex(value.asInstanceOf[CustomTree])
        acc.addEdge(stubOptionNode, value.asInstanceOf[CustomTree])
      }
    }

    def addListListNode(previous: CustomTree,
                        listListNode: List[List[CustomTree]],
                        acc: TreeGraph) = {
      val stubListListNode = ASTList()
      acc.addVertex(stubListListNode)
      acc.addEdge(previous, stubListListNode)
      listListNode.foreach(_.foreach(inner(stubListListNode, _, acc)))
    }

    def addCustomTerm = inner _

    def inner(previous: CustomTree,
              current: CustomTree,
              acc: TreeGraph):
        TreeGraph = {
      current match {

        case current @ CustomInit(tpe, name, argss) => {
          preliminary(previous, current, acc)

          // tpe
          addCustomTerm(current, tpe, acc)

          // name
          addCustomTerm(current, name, acc)

          // argss
          addListListNode(previous, argss, acc)

          acc
        }

        case current @ CustomSelf(name, decltpe) => {
          preliminary(previous, current, acc)

          // name
          addCustomTerm(current, name, acc)

          // decltpe
          addOptionNode(previous, decltpe, acc)

          acc
        }

        case current @ TypeParam(mods, name, tparams, tbounds, vbounds, cbounds) => {
          preliminary(previous, current, acc)

          // mods
          addListNode(previous, mods, acc)

          // name
          addSimpleNode(current, name, acc)

          // tparams
          addListNode(previous, tparams, acc)

          // tbounds
          addSimpleNode(current, tbounds, acc)

          // vbounds
          addListNode(previous, vbounds, acc)

          // cbounds
          addListNode(previous, cbounds, acc)

          acc
        }

        case current: TypeName => { // TypeName: add as is
          preliminary(previous, current, acc)

          acc
        }

        case current @ TypeApply(tpe, arg) => {
          preliminary(previous, current, acc)

          // tpe
          addSimpleNode(current, tpe, acc)

          // arg
          addListNode(previous, arg, acc)

          acc
        }

        case current @ Generator(pat, rhs) => {
          preliminary(previous, current, acc)

          // pat
          addSimpleNode(current, pat, acc)

          // rhs
          addCustomTerm(current, rhs, acc)

          acc
        }

        case current @ CaseGenerator(pat, rhs) => {
          preliminary(previous, current, acc)

          // pat
          addSimpleNode(current, pat, acc)

          // rhs
          addCustomTerm(current, rhs, acc)

          acc
        }

        case current @ Val(pat, rhs) => {
          preliminary(previous, current, acc)

          // pat
          addSimpleNode(current, pat, acc)

          // rhs
          addCustomTerm(current, rhs, acc)

          acc
        }

        case current @ Guard(cond) => {
          preliminary(previous, current, acc)

          // cond
          addCustomTerm(current, cond, acc)
        }

        case current: TermName => {  // Name: add as is
          preliminary(previous, current, acc)

          acc
        }

        case current @ TermParam(mods, name, decltpe, default) => {
          preliminary(previous, current, acc)

          // mods
          addListNode(previous, mods, acc)

          // name
          addSimpleNode(previous, name, acc)

          // decltpe
          addOptionNode(previous, decltpe, acc)

          // default
          addOptionNode(previous, default, acc)

          acc
        }

        case current @ TermLambda(param, body) => {
          preliminary(previous, current, acc)

          // param
          addListNode(previous, param, acc)

          // body
          addCustomTerm(current, body, acc)

          acc
        }

        case current @ TermSelect(term, name) => {
          preliminary(previous, current, acc)

          // term
          addCustomTerm(current, term, acc)

          // name
          addSimpleNode(current, name, acc)

          acc
        }

        case current @ TermInterpolate(prefix, parts, args) => {
          preliminary(previous, current, acc)

          // args
          addListNode(previous, args, acc)

          acc
        }

        case current @ TermApply(fun, args) => {
          preliminary(previous, current, acc)

          // fun
          addSimpleNode(current, fun, acc)

          // args
          addListNode(previous, args, acc)

          acc
        }

        case current @ TermApplyUsing(fun, args) => {
          preliminary(previous, current, acc)

          // fun
          addSimpleNode(current, fun, acc)

          // args
          addListNode(previous, args, acc)

          acc
        }

        case current @ TermApplyType(fun, targs) => {
          preliminary(previous, current, acc)

          // fun
          addSimpleNode(current, fun, acc)

          // targs
          addListNode(previous, targs, acc)

          acc
        }

        case current @ TermApplyInfix(lhs, op, targs, args) => {
          preliminary(previous, current, acc)

          inner(current, lhs.asInstanceOf[CustomTree], acc)

          // op
          addSimpleNode(current, op, acc)

          // targs
          addListNode(previous, targs, acc)

          // args
          addListNode(previous, args, acc)

          acc
        }

        case current @ TermApplyUnary(op, arg) => {
          preliminary(previous, current, acc)

          // op
          addSimpleNode(current, op, acc)

          // arg
          addCustomTerm(current, arg, acc)

          acc
        }

        case current @ TermAssign(lhs, rhs) => {
          preliminary(previous, current, acc)

          // lhs
          addCustomTerm(current, lhs, acc)

          // rhs
          addCustomTerm(current, rhs, acc)

          acc
        }

        case current @ TermReturn(expr) => {
          preliminary(previous, current, acc)

          // expr
          addCustomTerm(current, expr, acc)

          acc
        }

        case current @ TermNew(CustomInit(tpe, name, argss)) => {
          preliminary(previous, current, acc)

          // tpe
          addSimpleNode(current, tpe, acc)

          // name
          addSimpleNode(current, name, acc)

          // argss
          addListListNode(previous, argss, acc)

          acc
        }

        case current @ TermBlock(stats) => {
          preliminary(previous, current, acc)

          // stats
          addListNode(previous, stats, acc)

          acc
        }

        case current @ TermIf(cond, thenBranch, elseBranch) => {
          preliminary(previous, current, acc)

          // cond
          addCustomTerm(current, cond, acc)

          // thenBranch
          addCustomTerm(current, thenBranch, acc)

          // elseBranch
          addCustomTerm(current, elseBranch, acc)

          acc
        }

        case current @ TermTry(expr, catchp, finallyp) => {
          preliminary(previous, current, acc)

          // expr
          addCustomTerm(current, expr, acc)

          // catchp
          addListNode(previous, catchp, acc)

          // finallyp
          addOptionNode(previous, finallyp, acc)

          acc
        }

        case current @ TermWhile(cond, body) => {
          preliminary(previous, current, acc)

          // cond
          addCustomTerm(current, cond, acc)

          // body
          addCustomTerm(current, body, acc)

          acc
        }

        case current @ TermFor(iterator, body) => {
          preliminary(previous, current, acc)

          // iterator
          addListNode(previous, iterator, acc)

          // body
          addCustomTerm(current, body, acc)

          acc
        }

        case current @ TermThrow(expr) => {
          preliminary(previous, current, acc)

          // expr
          addCustomTerm(current, expr, acc)

          acc
        }

        case current: CustomLit => { // Literal: add as is
          preliminary(previous, current, acc)

          acc
        }

        case modifier: CustomMod => { // Modifier: add as is
          preliminary(previous, current, acc)

          acc
        }

        case current @ DefVal(mods, pats, decltpe, rhs) => {
          preliminary(previous, current, acc)

          // modifiers
          addListNode(previous, mods, acc)

          // patterns
          addListNode(previous, pats, acc)

          // typeOpt
          addOptionNode(previous, decltpe, acc)

          // term
          addCustomTerm(current, rhs, acc)
          acc
        }

        case current @ DefVar(mods, pats, decltpe, rhs) => {
          preliminary(previous, current, acc)

          // modifiers
          addListNode(previous, mods, acc)

          // patterns
          addListNode(previous, pats, acc)

          // typeOpt
          addOptionNode(previous, decltpe, acc)

          // term
          addOptionNode(previous, rhs, acc)

          acc
        }

        case current @ DefDef(mods, name, tparams, paramss, decltpe, body) => {
          preliminary(previous, current, acc)

          // mods
          addListNode(previous, mods, acc)

          // name
          addSimpleNode(current, name, acc)

          // tparams
          addListNode(previous, tparams, acc)

          // paramss
          addListListNode(previous, paramss, acc)

          // decltpe
          addOptionNode(previous, decltpe, acc)

          // stat
          addCustomTerm(current, body, acc)

          acc
        }

        case current @ DefEnum(mods, name, tparams, ctor, templ) => {
          preliminary(previous, current, acc)

          // mods
          addListNode(previous, mods, acc)

          // name
          addSimpleNode(previous, name, acc)

          // tparams
          addListNode(previous, tparams, acc)

          // ctor
          addSimpleNode(current, ctor, acc)

          // templ
          addCustomTerm(previous, templ, acc)

          acc
        }

        case current @ DefClass(mods, name, tparams, ctor, templ) => {
          preliminary(previous, current, acc)

          // mods
          addListNode(previous, mods, acc)

          // name
          addSimpleNode(current, name, acc)

          // tparams
          addListNode(previous, tparams, acc)

          // ctor
          addCustomTerm(current, ctor, acc)

          // templ
          addCustomTerm(current, templ, acc)

          acc
        }

        case current @ DefObject(mods, name, templ) => {
          preliminary(previous, current, acc)

          // mods
          addListNode(previous, mods, acc)

          // name
          addSimpleNode(current, name, acc)

          // templ
          addCustomTerm(current, templ, acc)

          acc
        }

        case current @ DefTrait(mods, name, tparams, ctor, templ) => {
          preliminary(previous, current, acc)

          // mods
          addListNode(previous, mods, acc)

          // name
          addSimpleNode(current, name, acc)

          // tparams
          addListNode(previous, tparams, acc)

          // ctor
          addSimpleNode(current, ctor, acc)

          // templ
          addCustomTerm(current, templ, acc)

          acc
        }

        case current @ CustomTemplate(early, inits, self, stats) => {
          preliminary(previous, current, acc)

          // early
          addListNode(previous, early, acc)
          
          // inits
          addListNode(previous, inits, acc)
          
          // self
          addCustomTerm(previous, self, acc)

          // stats
          addListNode(previous, stats, acc)

          acc
        }

        case current @ CustomSource(stats) => {
          preliminary(previous, current, acc)
 
          // stats
          addListNode(previous, stats, acc)

          acc
        }

        case current @ CustomPkg(_, stats) => {
          preliminary(previous, current, acc)

          // stats
          addListNode(previous, stats, acc)

          acc
        }

        case _: CustomImport =>
          // ignore imports for now
          acc

        case current @ PatVar(name: TermName) => {
          preliminary(previous, current, acc)

          // name
          addCustomTerm(current, name, acc)

          acc
        }

        case current: CustomName => { // Name: add as is
          preliminary(previous, current, acc)

          acc
        }

        case current @ PrimaryCtor(mods, name, paramss) => {
          preliminary(previous, current, acc)

          // mods
          addListNode(previous, mods, acc)

          // name
          addSimpleNode(current, name, acc)

          // paramss
          addListListNode(previous, paramss, acc)

          acc
        }


        case current @ SecondaryCtor(mods, name, paramss, init, stats) => {
          preliminary(previous, current, acc)

          // mods
          addListNode(previous, mods, acc)

          // name
          addSimpleNode(current, name, acc)

          // paramss
          addListListNode(previous, paramss, acc)

          // init
          addCustomTerm(current, init, acc)

          // stats
          addListNode(previous, stats, acc)

          acc
        }


        case otherwise: Any => {
          println(s"""You missed (referringMethodCollector/inner): $otherwise
                  which is: ${otherwise}""")
          acc
        }
      }
    }
    inner(tree, tree, new TreeGraph(classOf[DefaultEdge]))
  }


  def truncatedPrint(tree: CustomTree): String = {
    val string = tree.toString()
    string.slice(0, 15)+"..."
  }


  def truncate(string: String): String = {
    "\""+string.slice(0, 15)+"..."+"\""
  }


  def generateDOT(tree: CustomTree, filename: String): Unit = {
    val treeGraph = treeGraphToStringGraph(graphFromCustomTree(tree))
    val file = new File(filename)
    new DOTExporter[String, DefaultEdge](vertex => truncate(vertex)).exportGraph(treeGraph, file)
  }


  def defunIsInGraph(defun: DefDef, graph: TreeGraph) =
    graph.vertexSet
      .asScala
      .toList
      .filter(isDefun)
      .contains(defun)


  def customTreeFromGraph: TreeGraph => CustomTree = findRoot  // simple, ain't it?


  def mapOnGraph[A, B](graph: DirectedAcyclicGraph[A, DefaultEdge],
                       op: A => B):
      DirectedAcyclicGraph[B, DefaultEdge] = {
    val out = new DirectedAcyclicGraph[B, DefaultEdge](classOf[DefaultEdge])

    for (edge <- graph.edgeSet.asScala) {
      val source = graph.getEdgeSource(edge)
      val target = graph.getEdgeTarget(edge)
      val opSource = op(source)
      val opTarget = op(target)
      out.addVertex(op(source))
      out.addVertex(op(target))
      out.addEdge(opSource, opTarget)
    }
    out
  }


  def foldOverGraph[A, B](graph: DirectedAcyclicGraph[A, DefaultEdge],
                          op: (B, A) => B,
                          init: B): B = {
    val iterator = new BreadthFirstIterator(graph)
    var acc = init
    while (iterator.hasNext) {
      val elem = iterator.next
      acc = op(acc, elem)
    }
    acc
  }


  def treeGraphToStringGraph: TreeGraph => DirectedAcyclicGraph[String, DefaultEdge] =
    mapOnGraph(_, (tree: CustomTree) => tree.toString)
}
