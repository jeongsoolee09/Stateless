package kr.ac.korea.prl.stateless.Main

import kr.ac.korea.prl.stateless.TreeReader._
import kr.ac.korea.prl.stateless.TreeTraverser._
import kr.ac.korea.prl.stateless.TreeGraph._
import kr.ac.korea.prl.stateless.CustomTree._
import kr.ac.korea.prl.stateless.CustomTreeTranslator._

// I guess it's not best practice but...
import TreeReader._
import TreeTraverser._
import TreeGraph._
import CustomTreeTranslator._

import scala.meta._
import scala.meta.contrib._

object Main extends App {
  val dir =
    "/Users/jslee/Dropbox/Stateless/benchmarks/Scala/FunctionalImplementation/Bags/BagNotFunctional.scala"

  val bagTree: Tree = read(dir)
  val bagCustomTree = customTreeOfScalaMeta(bagTree)

  val vars = varCollector(bagCustomTree)

  val innerClassChain = innerClassChainCollector(bagCustomTree)

  val varReferringMethods = referringMethodCollector(bagCustomTree, vars)

  callerCollector(bagCustomTree, varReferringMethods(1))

  generateDOT(bagCustomTree, "BAG.dot")
}
