package kr.ac.korea.prl.stateless.TreeTraverser

import scala.annotation.tailrec
import scala.meta._

object ThisIsImpossible extends Exception {  }

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
                       case _           => false
                     })


  def isVar(tree: Tree): Boolean = tree match {
    case Defn.Var(_) => true
    case _ => false
  }

  // NOTE If some vars are missing, then you should first consult this definition
  def filterStatementList(statList: List[Stat]): List[Stat] = {
    val mapfunc = (elem: Stat) => elem match {
      /* ============ Decl ============ */
      case Decl.Var(_) => true
      /* ============ Defn ============ */
      case Defn.Var(_) => true
      case Defn.Def(_) => true
      case Defn.Class(_) => true
      case Defn.Trait(_) => true
      case Defn.Object(_) => true
      /* ============ Term ============ */
      case Term.Block(_) => true
      case Term.If(_) => true
      case Term.Try(_) => true
      case Term.While(_) => true
      case Term.For(_) => true
      case _ => false
    }
    statList.filter(mapfunc)
  }

  def varCollectorInner(tree: Tree, currentClass: Type.Name, currentMethod: Term.Name,
                         acc: List[(Type.Name, Term.Name, Term.Name)]):
      List[(Type.Name, Term.Name, Term.Name)] =
    tree match {
      /* ============ Decl ============ */
      case Decl.Var(_, List(Pat.Var(termName)), _) =>
        (currentClass, currentMethod, termName)::acc

      /* ============ Defn ============ */
      case Defn.Var(_, List(Pat.Var(termName)), _, _) =>
        (currentClass, currentMethod, termName)::acc

      case defn @ Defn.Def(_, termName, _, _, _, _) if (!isDefun(defn)) =>
        (currentClass, currentMethod, termName)::acc

      case Defn.Def(_, newMethodName, _, _, _, body) =>
        varCollectorInner(body, currentClass, newMethodName, acc)

      case Defn.Class(_, newClassName, _, _, Template(_, _, _, body)) =>
        body.foldLeft(List[(Type.Name, Term.Name, Term.Name)]())((a, elem) =>
          varCollectorInner(elem, newClassName, currentMethod, a)) ++ acc

      case Defn.Trait(_, _, _, _, templ) => templ match {
        case Template(_, _, _, stats) =>
          filterStatementList(stats).foldLeft(List[(Type.Name, Term.Name, Term.Name)]())((a, elem) =>
          varCollectorInner(elem, currentClass, currentMethod, a)) ++ acc
        case _                        => throw ThisIsImpossible
      }

      case Defn.Object(_, newObjectName, templ) => templ match {
        case Template(_, _, _, stats) =>
          filterStatementList(stats).foldLeft(List[(Type.Name, Term.Name, Term.Name)]())((a, elem) =>
          varCollectorInner(elem, currentClass, currentMethod, a)) ++ acc
        case _                        => throw ThisIsImpossible
      }

      /* ============ Term ============ */
      case Term.Block(stats) =>
        filterStatementList(stats).foldLeft(List[(Type.Name, Term.Name, Term.Name)]())((a, elem) =>
          varCollectorInner(elem, currentClass, currentMethod, a)) ++ acc

      case Term.If(_, thenBranch, elseBranch) => {
        val collectedFromThen = varCollectorInner(thenBranch, currentClass, currentMethod, List())
        val collectedFromElse = varCollectorInner(elseBranch, currentClass, currentMethod, List())
        collectedFromThen ++ collectedFromElse ++ acc
      }

      case Term.Try(tryStat, catchCases, finallyOpt) => finallyOpt match {
        case Some(finallyStat) => {
          val collectedFromTry = varCollectorInner(tryStat, currentClass, currentMethod, List())
          // val collectedFromCatch =
          val collectedFromFinally = varCollectorInner(finallyStat, currentClass, currentMethod, List())
          collectedFromTry ++ collectedFromFinally ++ acc
        }
        case None              => {
          val collectedFromTry = varCollectorInner(tryStat, currentClass, currentMethod, List())
          // val collectedFromCatch =
          collectedFromTry ++ acc
        }
      }

      case Term.While(_, body) =>
        varCollectorInner(body, currentClass, currentMethod, acc)

      case Term.For(_, body) =>
        varCollectorInner(body, currentClass, currentMethod, acc)

      /* ============ Otherwise ============ */
      case Source(stats) =>
        filterStatementList(stats).foldLeft(List[(Type.Name, Term.Name, Term.Name)]())((a, elem) =>
          varCollectorInner(elem, currentClass, currentMethod, a)) ++ acc

      case otherwise => {
        throw new MatchError(otherwise.productPrefix)
      }
    }

  /**
    * Collect all the vars in the given tree.
    *
    * @param tree
    * @return List of triples: ClassID * MethodID * VarID
    */
  def varCollector(tree: Tree): List[(Type.Name, Term.Name, Term.Name)] =
    varCollectorInner(tree,
                      Type.Name("_"), // placeholder
                      Term.Name("_"), // placeholder
                      List())
}
