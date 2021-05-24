package kr.ac.korea.prl.stateless.TreeReader

import scala.meta._

import java.nio.file.Files
import java.nio.file.Paths
import java.net.URI

class TreeReader {

  def read(uri: String): Tree = {
    // 일단은 하드코딩
    val path = Paths.get("", uri)
    val bytes = Files.readAllBytes(path)
    val text = new String(bytes, "UTF-8")
    val input = Input.VirtualFile(path.toString, text)
    val exampleTree = input.parse[Source].get
    exampleTree
  }
}
