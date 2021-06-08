package kr.ac.korea.prl.stateless.Main

import kr.ac.korea.prl.stateless.TreeReader._
import kr.ac.korea.prl.stateless.TreeTraverser._
import kr.ac.korea.prl.stateless.TreeGraph._

import scala.meta._
import scala.meta.contrib._

object Main extends App {
  val dir =
    "/Users/jslee/Dropbox/Stateless/benchmarks/Scala/DirectTranslation/DataStructures/Bags/Bag.scala"
  val bagTree: Tree = TreeReader.read(dir)

  TreeGraph.generateDOT(bagTree, "BAG.dot")
}
