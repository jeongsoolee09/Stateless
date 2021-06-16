package kr.ac.korea.prl.stateless.Main

import kr.ac.korea.prl.stateless.TreeReader._
import kr.ac.korea.prl.stateless.TreeTraverser._
import kr.ac.korea.prl.stateless.TreeGraph._
import kr.ac.korea.prl.stateless.CustomTree._

import scala.meta._
import scala.meta.contrib._

object Main extends App {
  val dir =
    "/Users/jslee/Dropbox/Stateless/benchmarks/Scala/FunctionalImplementation/Bags/BagNotFunctional.scala"
  val bagTree: Tree = TreeReader.read(dir)
  val bagCustomTree = CustomTreeTranslator.scalaMetaToCustomTree(bagTree)

  val vars = TreeTraverser.varCollector(bagTree)
  val innerClassChain = TreeTraverser.innerClassChainCollector(bagTree)
  val varReferringMethods = TreeTraverser.referringMethodCollector(bagTree, vars).toSet.toList

  val treeGraph = TreeGraph.graphFromCustomTree(bagCustomTree)

  treeGraph.vertexSet.contains(q"""def add(element: Element): Unit = {
                                 val oldfirst: Node[Element] = firstElement
                                 firstElement = new Node()
                                 firstElement.content = element
                                 firstElement.nextElement = oldfirst
                                 size += 1
                               }""")


  TreeTraverser.callerCollector(bagTree, varReferringMethods(1))

  // TreeGraph.generateDOT(bagTree, "BAG.dot")
}
