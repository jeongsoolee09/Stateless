package kr.ac.korea.prl.stateless.TreeGraph

import org.jgrapht.graph.DirectedAcyclicGraph
import org.jgrapht.traverse._
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.Graphs._
import org.jgrapht.nio.dot.DOTExporter

import java.io.File

import scala.meta._
import scala.meta.contrib._
import scala.collection.JavaConverters._

import kr.ac.korea.prl.stateless.CustomTree._

object TODO extends Exception

object TreeGraph {

  def isDefun(tree: CustomTree): Boolean = tree match {
    case DefDef(_, _, _, paramList, _, _) => !paramList.isEmpty
    case _                                  => false
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


  def graphFromCustomTree(tree: CustomTree):
      DirectedAcyclicGraph[CustomTree, DefaultEdge] = {

    def inner(previous: CustomTree,
              tree: CustomTree,
              acc: DirectedAcyclicGraph[CustomTree, DefaultEdge]):
        DirectedAcyclicGraph[CustomTree, DefaultEdge] = {
      tree match {

        case current @ CustomInit(tpe, name, argss) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // tpe
          inner(current, tpe, acc)

          // name
          inner(current, name, acc)

          // argss
          argss.foreach(_.foreach(inner(current, _, acc)))

          acc
        }

        case current @ CustomSelf(name, decltpe) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // name
          inner(current, name, acc)

          // decltpe
          if (!decltpe.isEmpty) {
            acc.addVertex(decltpe.get.asInstanceOf[CustomTree])
            acc.addEdge(current, decltpe.get.asInstanceOf[CustomTree])
          }

          acc
        }

        case current @ TypeParam(mods, name, tparams, tbounds, vbounds, cbounds) => {
          // TypeParameter: add as is
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          acc
        }

        case current: TypeName => {
          // TypeName: add as is
          acc.addVertex(current)
          if (!(previous equals current))
            acc.addEdge(previous, current)

          acc
        }

        case current @ TypeApply(tpe, arg) => {
          acc.addVertex(current)
          if (!(previous equals current))
            acc.addEdge(previous, current)

          // tpe
          acc.addVertex(tpe.asInstanceOf[CustomTree])
          acc.addEdge(current, tpe.asInstanceOf[CustomTree])

          // arg
          arg.foreach(inner(current, _, acc))

          acc
        }

        case current @ Generator(pat, rhs) => {
          acc.addVertex(current)
          if (!(previous equals current))
            acc.addEdge(previous, current)

          // pat
          acc.addVertex(pat.asInstanceOf[CustomTree])
          acc.addEdge(current, pat.asInstanceOf[CustomTree])

          // rhs
          inner(current, rhs, acc)

          acc
        }

        case current @ CaseGenerator(pat, rhs) => {
          acc.addVertex(current)
          if (!(previous equals current))
            acc.addEdge(previous, current)

          // pat
          acc.addVertex(pat.asInstanceOf[CustomTree])
          acc.addEdge(current, pat.asInstanceOf[CustomTree])

          // rhs
          inner(current, rhs, acc)

          acc
        }

        case current @ Val(pat, rhs) => {
          acc.addVertex(current)
          if (!(previous equals current))
            acc.addEdge(previous, current)

          // pat
          acc.addVertex(pat.asInstanceOf[CustomTree])
          acc.addEdge(current, pat.asInstanceOf[CustomTree])

          // rhs
          inner(current, rhs, acc)

          acc
        }

        case current @ Guard(cond) => {
          acc.addVertex(current)
          if (!(previous equals current))
            acc.addEdge(previous, current)

          // cond
          inner(current, cond, acc)
        }

        case current: TermName => {
          // Name: add as is
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          acc
        }

        case current @ TermParam(mods, name, decltpe, default) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // mods
          mods.foreach(inner(current, _, acc))

          // name
          acc.addVertex(name.asInstanceOf[CustomTree])
          acc.addEdge(current, name.asInstanceOf[CustomTree])

          // decltpe
          if (!decltpe.isEmpty) {
            acc.addVertex(decltpe.get.asInstanceOf[CustomTree])
            acc.addEdge(current, decltpe.get.asInstanceOf[CustomTree])
          }

          // default
          if (!default.isEmpty) {
            acc.addVertex(default.get.asInstanceOf[CustomTree])
            acc.addEdge(current, default.get.asInstanceOf[CustomTree])
          }

          acc
        }

        case current @ TermLambda(param, body) => {
          acc.addVertex(current)

          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // param
          param.foreach(param => inner(current, param.asInstanceOf[CustomTree], acc))

          // body
          inner(current, body, acc)
          acc
        }

        case current @ TermSelect(term, name) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          acc.addVertex(name.asInstanceOf[CustomTree])
          acc.addEdge(current, name.asInstanceOf[CustomTree])
          inner(current, term.asInstanceOf[CustomTree], acc)
          acc
        }

        case current @ TermInterpolate(prefix, parts, args) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // args
          args.foreach(inner(current, _, acc))
          acc
        }

        case current @ TermApply(fun, args) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // fun
          inner(current, fun, acc)

          // args
          args.foreach(inner(current, _, acc))
          acc
        }

        case current @ TermApplyUsing(fun, args) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)
          inner(current, fun.asInstanceOf[CustomTree], acc)

          // fun
          inner(current, fun, acc)

          // args
          args.foreach(inner(current, _, acc))
          acc
        }

        case current @ TermApplyType(fun, targs) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)
          inner(current, fun.asInstanceOf[CustomTree], acc)

          // fun
          inner(current, fun, acc)

          // args
          targs.foreach(inner(current, _, acc))
          acc
        }

        case current @ TermApplyInfix(lhs, op, targs, args) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)
          inner(current, lhs.asInstanceOf[CustomTree], acc)

          // op
          acc.addVertex(op.asInstanceOf[CustomTree])
          acc.addEdge(current, op.asInstanceOf[CustomTree])

          // targs
          targs.foreach(inner(current, _, acc))

          // args
          args.foreach(inner(current, _, acc))
          acc
        }

        case current @ TermApplyUnary(op, arg) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // op
          acc.addVertex(op.asInstanceOf[CustomTree])
          acc.addEdge(current, op.asInstanceOf[CustomTree])

          // arg
          inner(current, arg, acc)

          acc
        }

        case current @ TermAssign(lhs, rhs) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // lhs
          inner(current, lhs, acc)

          // rhs
          inner(current, rhs, acc)

          acc
        }

        case current @ TermReturn(expr) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // expr
          inner(current, expr, acc)

          acc
        }

        case current @ TermNew(CustomInit(tpe, name, argss)) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // tpe
          acc.addVertex(tpe.asInstanceOf[CustomTree])
          acc.addEdge(current, tpe.asInstanceOf[CustomTree])

          // name
          acc.addVertex(name.asInstanceOf[CustomTree])
          acc.addEdge(current, name.asInstanceOf[CustomTree])

          // argss
          argss.foreach(_.foreach(inner(current, _, acc)))

          acc
        }

        case current @ TermBlock(stats) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // stats
          stats.foreach(inner(current, _, acc))

          acc
        }

        case current @ TermIf(cond, thenBranch, elseBranch) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // cond
          inner(current, cond, acc)

          // thenBranch
          inner(current, thenBranch, acc)

          // elseBranch
          inner(current, elseBranch, acc)

          acc
        }

        case current @ TermTry(expr, catchp, finallyp) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // expr
          inner(current, expr, acc)

          // catchp
          catchp.foreach(inner(current, _, acc))

          // finallyp
          if (!finallyp.isEmpty) {
            inner(current, finallyp.get.asInstanceOf[CustomTree], acc)
          }

          acc
        }

        case current @ TermWhile(cond, body) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // cond
          inner(current, cond, acc)

          // body
          inner(current, body, acc)

          acc
        }

        case current @ TermFor(iterator, body) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // iterator
          iterator.foreach(inner(current, _, acc))

          // body
          inner(current, body, acc)

          acc
        }

        case current @ TermThrow(expr) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // expr
          inner(current, expr, acc)

          acc
        }

        case current: CustomLit => {
          // Literal: add as is
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)
          acc
        }

        case modifier: CustomMod => {
          // Modifier: add the productPrefix
          // modifiers cannot be given just by themselves
          acc.addVertex(modifier)
          acc.addEdge(previous, modifier)
          acc
        }

        case current @ DefVal(modifiers, patterns, typeOpt, term) => {
          acc.addVertex(current)

          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // modifiers
          modifiers.foreach(inner(current, _, acc))

          // patterns
          patterns.foreach(inner(current, _, acc))

          // typeOpt
          if (!typeOpt.isEmpty) {
            acc.addVertex(typeOpt.get.asInstanceOf[CustomTree])
            acc.addEdge(previous, typeOpt.get.asInstanceOf[CustomTree])
          }

          // term
          inner(current, term, acc)
          acc
        }

        case current @ DefVar(mods, pats, decltype, rhs) => {
          acc.addVertex(current)

          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // modifiers
          mods.foreach(inner(current, _, acc))

          // patterns
          pats.foreach(inner(current, _, acc))

          // decltype
          if (!decltype.isEmpty) {
            acc.addVertex(decltype.get.asInstanceOf[CustomTree])
            acc.addEdge(previous, decltype.get.asInstanceOf[CustomTree])
          }

          if (!rhs.isEmpty) {
            acc.addVertex(rhs.get.asInstanceOf[CustomTree])
            inner(current, rhs.get.asInstanceOf[CustomTree], acc)
          }
          acc
        }

        case current @ DefDef(mods, name, tparams, paramss, decltpe, body) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // mods
          mods.foreach(inner(current, _, acc))

          // name
          acc.addVertex(name)
          acc.addEdge(current, name.asInstanceOf[CustomTree])

          // tparams
          tparams.foreach(inner(current, _, acc))

          // paramss
          paramss.foreach(_.foreach(inner(current, _, acc)))

          // decltpe
          if (!decltpe.isEmpty) {
            acc.addVertex(decltpe.get.asInstanceOf[CustomTree])
            acc.addEdge(current, decltpe.get.asInstanceOf[CustomTree])
          }

          // stat
          inner(current, body, acc)

          acc
        }

        case current @ DefEnum(mods, name, tparams, ctor, templ) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // mods
          mods.foreach(inner(current, _, acc))

          // name
          acc.addVertex(name)
          acc.addEdge(current, name.asInstanceOf[CustomTree])

          // tparams
          tparams.foreach(inner(current, _, acc))

          // ctor
          acc.addVertex(ctor)
          acc.addEdge(current, ctor.asInstanceOf[CustomTree])

          // templ
          templ.stats.foreach(inner(current, _, acc))

          acc
        }


        case current @ DefClass(mods, name, tparams, ctor, templ) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // mods
          mods.foreach(inner(current, _, acc))

          // name
          acc.addVertex(name)
          acc.addEdge(current, name.asInstanceOf[CustomTree])

          // tparams
          tparams.foreach(inner(current, _, acc))

          // ctor
          inner(current, ctor.asInstanceOf[CustomTree], acc)

          // templ
          inner(current, templ.asInstanceOf[CustomTree], acc)

          acc
        }

        case current @ DefObject(mods, name, templ) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // mods
          mods.foreach(inner(current, _, acc))

          // name
          acc.addVertex(name)
          acc.addEdge(current, name.asInstanceOf[CustomTree])

          // templ
          inner(current, templ.asInstanceOf[CustomTree], acc)

          acc
        }

        case current @ DefTrait(mods, name, tparams, ctor, templ) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // mods
          mods.foreach(inner(current, _, acc))

          // name
          acc.addVertex(name)
          acc.addEdge(current, name.asInstanceOf[CustomTree])

          // tparams
          tparams.foreach(inner(current, _, acc))

          // ctor
          acc.addVertex(ctor)
          acc.addEdge(current, ctor.asInstanceOf[CustomTree])

          // templ
          templ.stats.foreach(inner(current, _, acc))

          acc
        }

        case current @ CustomTemplate(_, _, _, stats) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // stats
          stats.foreach(inner(current, _, acc))

          acc
        }

        case current @ CustomSource(stats) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // stats
          stats.foreach(inner(current, _, acc))

          acc
        }

        case current @ CustomPkg(_, stats) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // stats
          stats.foreach(inner(current, _, acc))

          acc
        }

        case _: CustomImport =>
          // ignore imports for now
          acc

        case current @ PatVar(name: TermName) => {
          acc.addVertex(current);
          if (!previous.equals(current))
            acc.addEdge(previous, current.asInstanceOf[CustomTree])

          // name
          inner(current, name, acc)

          acc
        }

        case current: CustomName => {
          // Name: add as is
          acc.addVertex(current);
          if (!previous.equals(current))
            acc.addEdge(previous, current.asInstanceOf[CustomTree])
          acc
        }

        case current @ PrimaryCtor(mods, name, paramss) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // mods
          mods.foreach(inner(current, _, acc))

          // name
          acc.addVertex(name)
          acc.addEdge(current, name)

          // paramss
          paramss.foreach(_.foreach(inner(current, _, acc)))

          acc
        }


        case current @ SecondaryCtor(mods, name, paramss, init, stats) => {
          acc.addVertex(current)
          if (!previous.equals(current))
            acc.addEdge(previous, current)

          // mods
          mods.foreach(inner(current, _, acc))

          // name
          acc.addVertex(name)
          acc.addEdge(current, name)

          // paramss
          paramss.foreach(_.foreach(inner(current, _, acc)))

          // init: ignored

          // stats
          stats.foreach(inner(current, _, acc))

          acc
        }

        case otherwise: Any => {
          println(s"""You missed (referringMethodCollector/inner): $otherwise
                  which is: ${otherwise}""")
          acc
        }
      }
    }
    inner(tree, tree, new DirectedAcyclicGraph[CustomTree, DefaultEdge](classOf[DefaultEdge]))
  }


  def truncatedPrint(tree: CustomTree): String = {
    val string = tree.toString()
    string.slice(0, 15)+"..."
  }


  def truncate(string: String): String = {
    "\""+string.slice(0, 15)+"..."+"\""
  }


  def treeGraphToStringGraph(graph: DirectedAcyclicGraph[CustomTree, DefaultEdge]):
      DirectedAcyclicGraph[String, DefaultEdge] = {
    val vertexList = graph.vertexSet.asScala.toList
    val vertexStringList = vertexList.map(vertex => truncate(vertex.toString))

    val out = new DirectedAcyclicGraph[String, DefaultEdge](classOf[DefaultEdge])

    for (edge <- graph.edgeSet.asScala) {
      val source = graph.getEdgeSource(edge)
      val target = graph.getEdgeTarget(edge)
      out.addVertex(source.toString)
      out.addVertex(target.toString)
      out.addEdge(source.toString, target.toString)
    }

    out
  }


  def generateDOT(tree: CustomTree, filename: String): Unit = {
    val treeGraph = treeGraphToStringGraph(graphFromCustomTree(tree))
    val file = new File(filename)
    new DOTExporter[String, DefaultEdge](vertex => truncate(vertex)).exportGraph(treeGraph, file)
  }


  def defunIsInGraph(defun: DefDef, graph: DirectedAcyclicGraph[CustomTree, DefaultEdge]) =
    graph.vertexSet()
      .asScala
      .toList
      .filter(isDefun)
      .contains(defun)
}
