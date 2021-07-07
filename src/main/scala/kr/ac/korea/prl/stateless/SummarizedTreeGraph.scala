/**
  * Scala source code <=> ScalaMeta Tree <=> CustomTree <=> TreeGraph <=> SummarizedTreeGraph
  *                                                                   ^^^
  */

package kr.ac.korea.prl.stateless.SummarizedTreeGraph

import kr.ac.korea.prl.stateless.TreeGraph.TreeGraph._
import kr.ac.korea.prl.stateless.CustomTree._

import org.jgrapht.graph.DefaultEdge
import org.jgrapht.graph.DirectedAcyclicGraph

import scala.collection.JavaConverters._

object SummarizedTreeGraph {

  type TreeGraph = DirectedAcyclicGraph[CustomTree, DefaultEdge]
  type StringGraph = DirectedAcyclicGraph[(String, Int), DefaultEdge]


  val summarizeCustomTree: CustomTree => String =
    (ctree: CustomTree) => ctree match {
      case current: CustomInit => current.productPrefix
      case current: CustomSelf => current.productPrefix
      case current: TypeParam => current.toString  // atom
      case current: TypeName => current.toString  // atom
      case current: TypeApply => current.productPrefix
      case current: TypeBounds => current.productPrefix
      case current: Generator => current.productPrefix
      case current: CaseGenerator => current.productPrefix
      case current: Val => current.productPrefix
      case current: Guard => current.productPrefix
      case current: TermName => current.toString  // atom
      case current: TermParam => current.productPrefix
      case current: TermLambda => current.productPrefix
      case current: TermSelect => current.productPrefix
      case current: TermInterpolate => current.productPrefix
      case current: TermApply => current.productPrefix
      case current: TermApplyUsing => current.productPrefix
      case current: TermApplyType => current.productPrefix
      case current: TermApplyInfix => current.productPrefix
      case current: TermApplyUnary => current.productPrefix
      case current: TermAssign => current.productPrefix
      case current: TermReturn => current.productPrefix
      case current: TermNew => current.productPrefix
      case current: TermBlock => current.productPrefix
      case current: TermIf => current.productPrefix
      case current: TermTry => current.productPrefix
      case current: TermWhile => current.productPrefix
      case current: TermFor => current.productPrefix
      case current: TermThrow => current.productPrefix
      case current: TermSuper => current.productPrefix
      case current: TermThis => current.productPrefix
      case current: LitNull => current.toString // atom
      case current: LitInt => current.toString  // atom
      case current: LitDouble => current.toString  // atom
      case current: LitFloat => current.toString  // atom
      case current: LitByte => current.toString  // atom
      case current: LitShort => current.toString  // atom
      case current: LitChar => current.toString  // atom
      case current: LitLong => current.toString  // atom
      case current: LitBoolean => current.toString  // atom
      case current: LitUnit => current.toString  // atom
      case current: LitString => current.toString  // atom
      case current: LitSymbol => current.toString  // atom
      case current: Private => current.productPrefix
      case current: Protected => current.productPrefix
      case current: Implicit => current.productPrefix
      case current: Abstract => current.productPrefix
      case current: Override => current.productPrefix
      case current: Super => current.productPrefix
      case current: Final => current.productPrefix
      case current: DefVal => current.productPrefix
      case current: DefVar => current.productPrefix
      case current: DefDef => current.productPrefix
      case current: DefEnum => current.productPrefix
      case current: DefClass => current.productPrefix
      case current: DefObject => current.productPrefix
      case current: DefTrait => current.productPrefix
      case current: CustomTemplate => current.productPrefix
      case current: CustomSource => current.productPrefix
      case current: CustomPkg => current.productPrefix
      case current: CustomImport => current.productPrefix
      case current: PatVar => current.productPrefix
      case current: CustomName => current.toString  // atom
      case current: PrimaryCtor => current.productPrefix
      case current: SecondaryCtor => current.productPrefix
      case current: CustomCase => current.productPrefix
      case current: CustomImporter => current.productPrefix
      case current: DeclDef => current.productPrefix
      case current: DeclVar => current.productPrefix
      case current: DeclVal => current.productPrefix
      case current: ImporteeGiven => current.productPrefix
      case current: ImporteeGivenAll => current.productPrefix
      case current: ImporteeName => current.productPrefix
      case current: ImporteeWildcard => current.productPrefix
    }


  def summarizeTreeGraph(originalGraph: TreeGraph): StringGraph = {
    case class TooManyMatches(content: String) extends Exception

    def searchTupleNodeByCustomTree(ctree: CustomTree,
                                    nodes: List[(CustomTree, Int)]): (CustomTree, Int) =
      nodes.foldLeft[Option[(CustomTree, Int)]](None)((acc: Option[(CustomTree, Int)],
                                                       elem: (CustomTree, Int)) =>
        if (elem._1 equals ctree)
          acc match {
            case None => Some(elem)
            case Some(_) => throw TooManyMatches(ctree.toString)
          } else acc).get


    def getParents[A](from: DirectedAcyclicGraph[A, DefaultEdge],
                      ofThis: A): List[A] =
      from.incomingEdgesOf(ofThis).asScala.toList.map(from.getEdgeSource(_))


    val foldfunc1 = (acc: (DirectedAcyclicGraph[(CustomTree, Int), DefaultEdge], Int),
                     elem: CustomTree) => {
      val accGraph = acc._1
      val counter = acc._2
      val elemVertex = (elem, counter)
      accGraph.addVertex(elemVertex)
      for (parent <- getParents(originalGraph, elem)) {
        val parentTup = searchTupleNodeByCustomTree(parent, accGraph.vertexSet.asScala.toList)
        accGraph.addEdge(parentTup, elemVertex)
      }
      (accGraph, counter+1)
    }

    val foldedGraph = foldOverGraph(originalGraph, foldfunc1,
                                    (new DirectedAcyclicGraph[(CustomTree, Int), DefaultEdge]
                                       (classOf[DefaultEdge]), 0))._1

    val mapfunc = (tup: (CustomTree, Int)) => (summarizeCustomTree(tup._1), tup._2)

    val mappedGraph = mapOnGraph(foldedGraph, mapfunc)

    // postcondition check: vertexSet's size should not change
    assert(originalGraph.vertexSet.size == foldedGraph.vertexSet.size)
    assert(foldedGraph.vertexSet.size == mappedGraph.vertexSet.size)

    // postcondition check: edgeSet's size should not change
    assert(originalGraph.edgeSet.size == foldedGraph.edgeSet.size)
    assert(foldedGraph.edgeSet.size == mappedGraph.edgeSet.size)

    return mappedGraph
  }


  def desummarizeStringGraph(summarizedGraph: StringGraph): TreeGraph = {
    ???
  }

}
