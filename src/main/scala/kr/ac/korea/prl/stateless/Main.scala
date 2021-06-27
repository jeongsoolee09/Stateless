package kr.ac.korea.prl.stateless.Main

import kr.ac.korea.prl.stateless.TreeReader._
import kr.ac.korea.prl.stateless.TreeTraverser._
import kr.ac.korea.prl.stateless.TreeGraph._
import kr.ac.korea.prl.stateless.CustomTree._
import kr.ac.korea.prl.stateless.CustomTreeTranslator._

import scala.meta._
import scala.meta.contrib._

object Main extends App {
  val dir =
    "/Users/jslee/Dropbox/Stateless/benchmarks/Scala/FunctionalImplementation/Bags/BagNotFunctional.scala"

  val bagTree: Tree = TreeReader.read(dir)
  val bagCustomTree = CustomTreeTranslator.scalaMetaToCustomTree(bagTree)

  val vars = TreeTraverser.varCollector(bagCustomTree)

  val innerClassChain = TreeTraverser.innerClassChainCollector(bagCustomTree)

  val varReferringMethods = TreeTraverser.referringMethodCollector(bagCustomTree, vars)

  TreeTraverser.callerCollector(bagCustomTree, varReferringMethods(1))

  // TreeGraph.generateDOT(bagTree, "BAG.dot")
}
