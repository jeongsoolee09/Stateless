import org.scalatest._
import flatspec._
import matchers._

import kr.ac.korea.prl.stateless.TreeTraverser
import kr.ac.korea.prl.stateless.TreeReader
import scala.meta._
import scala.meta.contrib._


class ExampleSpec extends AnyFlatSpec with should.Matchers {

  /* Equality Predicates ============================== */
  /* ================================================== */

  def typeOptEquals(typeOpt1: Option[Type], typeOpt2: Option[Type]): Boolean =
    (typeOpt1, typeOpt2) match {
      case (None, None)               => true
      case (Some(_), None)            => false
      case (None, Some(_))            => false
      case (Some(type1), Some(type2)) => type1.isEqual(type2)
  }


  def termOptEquals(typeOpt1: Option[Term], typeOpt2: Option[Term]): Boolean =
    (typeOpt1, typeOpt2) match {
      case (None, None)               => true
      case (Some(_), None)            => false
      case (None, Some(_))            => false
      case (Some(type1), Some(type2)) => type1.isEqual(type2)
    }


  def tupleListEquals(treeList1: List[(Type.Name, Term.Name, Term.Name,
                                       Option[Type], Option[Term])],
                      treeList2: List[(Type.Name, Term.Name, Term.Name,
                                       Option[Type], Option[Term])]): Boolean = {
    if (treeList1.length != treeList2.length) false
    else {
      val zipped = treeList1.zip(treeList2)
      zipped.foldLeft(true)((acc: Boolean,
                             tup: ((Type.Name, Term.Name, Term.Name, Option[Type], Option[Term]),
                                   (Type.Name, Term.Name, Term.Name, Option[Type], Option[Term]))) =>
        acc && tup._1._1.isEqual(tup._2._1)
          && tup._1._2.isEqual(tup._2._2)
          && tup._1._3.isEqual(tup._2._3)
          && typeOptEquals(tup._1._4, tup._2._4)
          && termOptEquals(tup._1._5, tup._2._5)
      )
    }
  }


  /* Unit Tests ======================================= */
  /* ================================================== */


  // "varCollector" should "collect all var in a given tree" in {

  //   /* Test 1: simple example where there are three vars  */
  //   /* ================================================== */

  //   val threeVars =
  //     q"""def f(): Unit = {var x: Int = 1
  //         var y: Int = 2
  //         y = 100
  //         var z: Int = 3}"""
  //   tupleListEquals(TreeTraverser.varCollector(threeVars),
  //                   List((Type.Name("ph"), Term.Name("f"), Term.Name("z"), Some(Type.Name("Int")), Some(Lit.Int(1))),
  //                        (Type.Name("ph"), Term.Name("f"), Term.Name("y"), Some(Type.Name("Int")), Some(Lit.Int(2))),
  //                        (Type.Name("ph"), Term.Name("f"), Term.Name("x"), Some(Type.Name("Int")), Some(Lit.Int(3))))) should be (true)


  // }
}
