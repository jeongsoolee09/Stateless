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

object TODO extends Exception

object TreeGraph {

  def isDefun(tree: Tree): Boolean = tree match {
    case Defn.Def(_, _, _, paramList, _, _) => !paramList.isEmpty
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


  def graphFromMetaTree(tree: Tree): DirectedAcyclicGraph[Tree, DefaultEdge] = {

    def inner(previous: Tree,
              tree: Tree,
              acc: DirectedAcyclicGraph[Tree, DefaultEdge]):
        DirectedAcyclicGraph[Tree, DefaultEdge] = {
      tree match {

        case current @ Type.Param(mods, name, tparasm, tbounds, vbounds, cbounds) => {
          // TypeParameter: add as is
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)
          acc
        }

        case current: Term.Name => {
          // Name: add as is
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)
          acc
        }

        case current @ Term.Param(mods, name, decltpe, default) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          // mods
          mods.foreach(inner(current, _, acc))

          // name
          acc.addVertex(name.asInstanceOf[Tree])
          acc.addEdge(current, name.asInstanceOf[Tree])

          // decltpe
          if (!decltpe.isEmpty) {
            acc.addVertex(decltpe.get.asInstanceOf[Tree])
            acc.addEdge(current, decltpe.get.asInstanceOf[Tree])
          }

          // default
          if (!default.isEmpty) {
            acc.addVertex(default.get.asInstanceOf[Tree])
            acc.addEdge(current, default.get.asInstanceOf[Tree])
          }

          acc
        }

        case current: Lit => {
          // Literal: add as is
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)
          acc
        }

        case modifier: Mod => {
          // Modifier: add the productPrefix
          // modifiers cannot be given just by themselves
          acc.addVertex(modifier)
          acc.addEdge(previous, modifier)
          acc
        }

        case current @ Defn.Val(modifiers, patterns, typeOpt, term) => {
          acc.addVertex(current)

          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          // modifiers
          modifiers.foreach(inner(current, _, acc))

          // patterns
          patterns.foreach(inner(current, _, acc))

          // typeOpt
          if (!typeOpt.isEmpty) {
            acc.addVertex(typeOpt.get.asInstanceOf[Tree])
            acc.addEdge(previous, typeOpt.get.asInstanceOf[Tree])
          }

          // term
          inner(current, term, acc)
          acc
        }

        case current @ Term.Function(param, body) => {
          acc.addVertex(current)

          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          // param
          param.foreach(param => inner(current, param.asInstanceOf[Tree], acc))

          // body
          inner(current, body, acc)
          acc
        }


        case current @ Defn.Var(mods, pats, decltype, rhs) => {
          acc.addVertex(current)

          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          // modifiers
          mods.foreach(inner(current, _, acc))

          // patterns
          pats.foreach(inner(current, _, acc))

          // decltype
          if (!decltype.isEmpty) {
            acc.addVertex(decltype.get.asInstanceOf[Tree])
            acc.addEdge(previous, decltype.get.asInstanceOf[Tree])
          }

          if (!rhs.isEmpty) {
            acc.addVertex(rhs.get.asInstanceOf[Tree])
            inner(current, rhs.get.asInstanceOf[Tree], acc)
          }
          acc
        }

        case current @ Term.Select(term, name) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          acc.addVertex(name.asInstanceOf[Tree])
          acc.addEdge(current, name.asInstanceOf[Tree])
          inner(current, term.asInstanceOf[Tree], acc)
          acc
        }

        case current @ Term.Apply(fun, args) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)
          inner(current, fun.asInstanceOf[Tree], acc)
          args.foreach(inner(current, _, acc))
          acc
        }

        case current @ Term.ApplyUsing(fun, args) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)
          inner(current, fun.asInstanceOf[Tree], acc)

          // args
          args.foreach(inner(current, _, acc))
          acc
        }

        case current @ Term.ApplyInfix(lhs, op, targs, args) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)
          inner(current, lhs.asInstanceOf[Tree], acc)

          // op
          acc.addVertex(op.asInstanceOf[Tree])
          acc.addEdge(current, op.asInstanceOf[Tree])

          // targs
          targs.foreach(inner(current, _, acc))

          // args
          args.foreach(inner(current, _, acc))
          acc
        }

        case current @ Term.ApplyUnary(op, arg) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          // op
          acc.addVertex(op.asInstanceOf[Tree])
          acc.addEdge(current, op.asInstanceOf[Tree])

          // arg
          inner(current, arg, acc)

          acc
        }

        case current @ Term.Assign(lhs, rhs) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          // lhs
          inner(current, lhs, acc)

          // rhs
          inner(current, rhs, acc)

          acc
        }

        case current @ Term.Return(expr) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          // expr
          inner(current, expr, acc)

          acc
        }

        case current @ Term.New(Init(tpe, name, argss)) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          // tpe
          acc.addVertex(tpe.asInstanceOf[Tree])
          acc.addEdge(current, tpe.asInstanceOf[Tree])

          // name
          acc.addVertex(name.asInstanceOf[Tree])
          acc.addEdge(current, name.asInstanceOf[Tree])

          // argss
          argss.foreach(_.foreach(inner(current, _, acc)))

          acc
        }

        case current @ Term.Block(stats) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          // stats
          stats.foreach(inner(current, _, acc))

          acc
        }

        case current @ Term.If(cond, thenBranch, elseBranch) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          // cond
          inner(current, cond, acc)

          // thenBranch
          inner(current, thenBranch, acc)

          // elseBranch
          inner(current, elseBranch, acc)

          acc
        }

        case current @ Term.Try(expr, catchp, finallyp) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          // expr
          inner(current, expr, acc)

          // catchp
          catchp.foreach(inner(current, _, acc))

          // finallyp
          if (!finallyp.isEmpty) {
            inner(current, finallyp.get.asInstanceOf[Tree], acc)
          }

          acc
        }

        case current @ Term.While(cond, body) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          // cond
          inner(current, cond, acc)

          // body
          inner(current, body, acc)

          acc
        }

        case current @ Term.For(iterator, body) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          // iterator
          iterator.foreach(inner(current, _, acc))

          // body
          inner(current, body, acc)

          acc
        }

        case current @ Term.Throw(expr) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          // expr
          inner(current, expr, acc)

          acc
        }

        case current @ Defn.Def(mods, name, tparams, paramss, decltpe, body)
            if isDefun(current) => {
              acc.addVertex(current)
              if (!previous.isEqual(current))
                acc.addEdge(previous, current)

              // mods
              mods.foreach(inner(current, _, acc))

              // name
              acc.addVertex(name)
              acc.addEdge(current, name.asInstanceOf[Tree])

              // tparams
              tparams.foreach(inner(current, _, acc))

              // paramss
              paramss.foreach(_.foreach(inner(current, _, acc)))

              // decltpe
              if (!decltpe.isEmpty) {
                acc.addVertex(decltpe.get.asInstanceOf[Tree])
                acc.addEdge(current, decltpe.get.asInstanceOf[Tree])
              }

              // stat
              inner(current, body, acc)

              acc
            }

        case current @ Ctor.Primary(mods, name, paramss) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
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


          case current @ Ctor.Secondary(mods, name, paramss, init, stats) => {
            acc.addVertex(current)
            if (!previous.isEqual(current))
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

        case current @ Template(_, _, _, stats) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          // stats
          stats.foreach(inner(current, _, acc))

          acc
        }

        case current @ Defn.Class(mods, name, tparams, ctor, templ) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          // mods
          mods.foreach(inner(current, _, acc))

          // name
          acc.addVertex(name)
          acc.addEdge(current, name.asInstanceOf[Tree])

          // tparams
          tparams.foreach(inner(current, _, acc))

          // ctor
          inner(current, ctor.asInstanceOf[Tree], acc)

          // templ
          inner(current, templ.asInstanceOf[Tree], acc)

          acc
        }

        case current @ Defn.Object(mods, name, templ) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          // mods
          mods.foreach(inner(current, _, acc))

          // name
          acc.addVertex(name)
          acc.addEdge(current, name.asInstanceOf[Tree])

          // templ
          inner(current, templ.asInstanceOf[Tree], acc)

          acc
        }

        case current @ Source(stats) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          // stats
          stats.foreach(inner(current, _, acc))

          acc
        }

        case current @ Pkg(_, stats) => {
          acc.addVertex(current)
          if (!previous.isEqual(current))
            acc.addEdge(previous, current)

          // stats
          stats.foreach(inner(current, _, acc))

          acc
        }

        case _: Import =>
          // ignore imports
          acc

        case current: Pat => {
          // Pattern: add as is
          acc.addVertex(current);
          if (!previous.isEqual(current))
            acc.addEdge(previous, current.asInstanceOf[Tree])
          acc
        }

        case otherwise: Any => {
          println(s"""You missed (referringMethodCollector/inner): $otherwise
                  which is: ${otherwise.productPrefix}""")
          acc
        }
      }
    }
    inner(tree, tree, new DirectedAcyclicGraph[Tree, DefaultEdge](classOf[DefaultEdge]))
  }


  def truncatedPrint(tree: Tree): String = {
    val string = tree.toString()
    string.slice(0, 15)+"..."
  }


  def truncate(string: String): String = {
    "\""+string.slice(0, 15)+"..."+"\""
  }


  def treeGraphToStringGraph(graph: DirectedAcyclicGraph[Tree, DefaultEdge]):
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


  def generateDOT(tree: Tree, filename: String): Unit = {
    val treeGraph = treeGraphToStringGraph(graphFromMetaTree(tree))
    val file = new File(filename)
    new DOTExporter[String, DefaultEdge](vertex => truncate(vertex)).exportGraph(treeGraph, file)
  }
}