package kr.ac.korea.prl.stateless.Main

import kr.ac.korea.prl.stateless.TreeReader
import kr.ac.korea.prl.stateless.TreeTraverser

import scala.meta._
import scala.meta.contrib._

object Main extends App {
  val treeTraverser = new TreeTraverser.TreeTraverser()
  val treeReader = new TreeReader.TreeReader()
  val dir =
    "/Users/jslee/Dropbox/Stateless/benchmarks/Scala/DirectTranslation/DataStructures/Bags/Bag.scala"
  val bagTree: Tree = treeReader.read(dir)

  val vars = treeTraverser.varCollector(bagTree)
  val innerClassChain = treeTraverser.innerClassChainCollector(bagTree)
  val varReferringMethods = treeTraverser.referringMethodCollector(bagTree, vars)
  println("varCollector: " + vars)
  println("innerClassChainCollector: " + innerClassChain)
  println("varReferringMethods: " + varReferringMethods)
}
