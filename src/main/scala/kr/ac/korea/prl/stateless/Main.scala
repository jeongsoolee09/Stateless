package kr.ac.korea.prl.stateless.Main

import kr.ac.korea.prl.stateless.TreeReader

object Main extends App {
  val tree = new TreeReader.TreeReader()
  val dir = "/Users/jslee/Dropbox/Stateless/benchmarks/Scala/DirectTranslation/DataStructures/Bags/Bag.scala"
  println(tree.read(dir))

}
