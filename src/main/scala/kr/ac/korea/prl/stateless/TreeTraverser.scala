package kr.ac.korea.prl.stateless.TreeTraverser

import scala.annotation.tailrec
import scala.meta._

class TreeTraverser {

  /**
    * Is this tree a Function Definition?
    *
    * @param tree
    * @return if function definition then true, else false
    */
  def isDefun(tree: Tree): Boolean = tree match {
    case Defn.Def(_, _, _, paramList, _, _) => !paramList.isEmpty
    case _                                  => false
  }


  def filterVar(paramList: List[Defn]): List[Defn] =
    paramList.filter(defn => defn match {
                       case Defn.Var(_) => true
                       case _ => false
                     })


  def isVar(tree: Tree): Boolean = tree match {
    case Defn.Var(_) => true
    case _ => false
  }

  def varCollectorInner(tree: Tree, currentClass: Type.Name, currentMethod: Term.Name,
                         acc: List[(Type.Name, Term.Name, Term.Name)]):
      List[(Type.Name, Term.Name, Term.Name)] =
    tree match {
      case Defn.Var(_, List(Pat.Var(termName)), _, _) =>
        (currentClass, currentMethod, termName)::acc
      case Defn.Class(_, newClassName, _, _, Template(_, _, _, body)) =>
        body.foldLeft(List[(Type.Name, Term.Name, Term.Name)]())((a, elem) =>
          varCollectorInner(elem, newClassName, currentMethod, a)) ++ acc
      case defn @ Defn.Def(_, newMethodName, _, paramList, _, body) if isDefun(defn) =>
        varCollectorInner(body, currentClass, newMethodName, acc)
      case Term.Block(statementList) =>
        statementList.foldLeft(List[(Type.Name, Term.Name, Term.Name)]())((a, elem) =>
          varCollectorInner(elem, currentClass, currentMethod, a)) ++ acc
    }

  def varCollector(tree: Tree): List[(Type.Name, Term.Name, Term.Name)] =
    varCollectorInner(tree, Type.Name("_"), Term.Name("_"), List())

  val add = q"def add(x:Int, y:Int) = x+y"
  val add1 = q"""def add(x:Int, y:Int) = {
  var a: Int = x+1
  var b: Int = y+1
  a+b
}"""
  val val_ = q"""val x: Int = 1"""
  varCollector(add1)
}
