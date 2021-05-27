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


  /**
    * Is this statement interesting?
    *
    * @param elem
    * @return true if the given statement is interesting, false otherwise
    */
  def isInteresting(elem: Stat) = elem match {
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
    /* ============ Source ============ */
    case Source(_) => true
    case Pkg(_) => true
    case otherwise => // println("Not Interesting: " + otherwise.productPrefix);
      false
  }


  def functionBodyIsComposite(statPossiblyBlock: Term): Boolean = statPossiblyBlock match {
    case Term.Block(_) => true
    case Term.If(_)    => true
    case Term.Try(_)   => true
    case Term.While(_) => true
    case Term.For(_)   => true
    case _             => false
  }


  def varCollectorInner(tree: Tree, currentClass: Type.Name, currentMethod: Term.Name,
                        acc: List[(Type.Name, Term.Name, Term.Name, Option[Type], Option[Term])]):
      List[(Type.Name, Term.Name, Term.Name, Option[Type], Option[Term])] =
    tree match {
      /* ============ Decl ============ */
      case Decl.Var(_, List(Pat.Var(termName)), valType) =>
        (currentClass, currentMethod, termName, Some(valType), None)::acc

      /* ============ Defn ============ */
      case Defn.Var(_, List(Pat.Var(termName)), typeOpt, value) =>
        (currentClass, currentMethod, termName, typeOpt, value)::acc

      case defn @ Defn.Def(_) if (!isDefun(defn)) =>
        acc

      case Defn.Def(_, newMethodName, _, _, _, body) =>  // Defun case
        if (functionBodyIsComposite(body))
          varCollectorInner(body, currentClass, newMethodName, acc)
        else
          acc

      case Defn.Class(_, newClassName, _, _, Template(_, _, _, body)) =>
        body.foldLeft(List[(Type.Name, Term.Name, Term.Name,
                            Option[Type], Option[Term])]())((a, elem) =>
          varCollectorInner(elem, newClassName, currentMethod, a)) ++ acc

      case Defn.Trait(_, _, _, _, templ) => templ match {
        case Template(_, _, _, stats) =>
          stats.filter(isInteresting).foldLeft(List[(Type.Name, Term.Name, Term.Name,
                                                     Option[Type], Option[Term])]())((a, elem) =>
            varCollectorInner(elem, currentClass, currentMethod, a)) ++ acc
        case _                        => throw ThisIsImpossible
      }

      case Defn.Object(_, newObjectName, templ) => templ match {
        case Template(_, _, _, stats) =>
          stats.filter(isInteresting).foldLeft(List[(Type.Name, Term.Name, Term.Name,
                                                    Option[Type], Option[Term])]())((a, elem) =>
            varCollectorInner(elem, currentClass, currentMethod, a)) ++ acc
        case _                        => throw ThisIsImpossible
      }

      /* ============ Term ============ */
      case Term.Block(stats) =>
        stats.filter(isInteresting).foldLeft(List[(Type.Name, Term.Name, Term.Name,
                                                  Option[Type], Option[Term])]())((a, elem) =>
          varCollectorInner(elem, currentClass, currentMethod, a)) ++ acc

      case Term.If(_, thenBranch, elseBranch) => {
        val collectedFromThen = if (isInteresting(thenBranch))
                                  varCollectorInner(thenBranch, currentClass, currentMethod, List())
                                else List()
        val collectedFromElse = if (isInteresting(elseBranch))
                                  varCollectorInner(elseBranch, currentClass, currentMethod, List())
                                else List()
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

      /* ============ Source ============ */
      case Source(stats) =>
        stats.filter(isInteresting).foldLeft(List[(Type.Name, Term.Name, Term.Name,
                                                  Option[Type], Option[Term])]())((a, elem) =>
          varCollectorInner(elem, currentClass, currentMethod, a)) ++ acc

      /* ============ Source ============ */
      case Pkg(_, stats) =>
        stats.filter(isInteresting).foldLeft(List[(Type.Name, Term.Name, Term.Name,
                                                  Option[Type], Option[Term])]())((a, elem) =>
          varCollectorInner(elem, currentClass, currentMethod, a)) ++ acc

      case otherwise => {
        println(otherwise);
        throw new MatchError(otherwise.productPrefix)
      }
    }


  /**
    * Collect all the vars in the given tree.
    *
    * @param tree
    * @return List of triples: ClassID * MethodID * VarID
    */
  def varCollector(tree: Tree): List[(Type.Name, Term.Name, Term.Name,
                                      Option[Type], Option[Term])] =
    varCollectorInner(tree,
                      Type.Name("ph"), // placeholder
                      Term.Name("ph"), // placeholder
                      List())
}
