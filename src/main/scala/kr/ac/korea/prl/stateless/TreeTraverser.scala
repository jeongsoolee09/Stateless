package kr.ac.korea.prl.stateless.TreeTraverser

import scala.annotation.tailrec
import scala.meta._
import scala.meta.contrib._
import java.io.InvalidClassException

object ThisIsImpossible extends Exception

object TODO extends Exception

class TreeTraverser {

  def truncatedPrint(tree: Tree): String = {
    val string = tree.toString()
    string.slice(0, 15)+"..."
  }

  /**
    * Vanilla tree traverser provided by ScalaMeta.
    *
    * @param tree
    * @return
    */
  def traverse(tree: Tree): Unit = tree.traverse {
    case node =>
      println(s"${node.productPrefix}")
  }

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
    case otherwise => false
  }


  def functionBodyIsComposite(statPossiblyBlock: Term): Boolean =
    statPossiblyBlock match {
      case Term.Block(_) => true
      case Term.If(_)    => true
      case Term.Try(_)   => true
      case Term.While(_) => true
      case Term.For(_)   => true
      case _             => false
    }


  /**
    * Collect all the vars in the given tree.
    *
    * @param tree
    * @return List of triples: ClassID * MethodID * VarID * TypeOption * TermOption
    */
  def varCollector(tree: Tree): List[(Type.Name, Term.Name, Term.Name,
                                      Option[Type], Option[Term])] = {

    def inner(tree: Tree, currentClass: Type.Name, currentMethod: Term.Name,
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
            inner(body, currentClass, newMethodName, acc)
          else
            acc

        case Defn.Class(_, newClassName, _, _, Template(_, _, _, body)) =>
          body.foldLeft(List[(Type.Name, Term.Name, Term.Name,
                              Option[Type], Option[Term])]())((a, elem) =>
            inner(elem, newClassName, currentMethod, List())++a) ++ acc

        case Defn.Trait(_, _, _, _, templ) => templ match {
          case Template(_, _, _, stats) =>
            stats.filter(isInteresting).foldLeft(List[(Type.Name, Term.Name, Term.Name,
                                                       Option[Type], Option[Term])]())((a, elem) =>
              inner(elem, currentClass, currentMethod, List())++a) ++ acc
          case _                        => throw ThisIsImpossible
        }

        case Defn.Object(_, newObjectName, templ) => templ match {
          case Template(_, _, _, stats) =>
            stats.filter(isInteresting).foldLeft(List[(Type.Name, Term.Name, Term.Name,
                                                       Option[Type], Option[Term])]())((a, elem) =>
              inner(elem, currentClass, currentMethod, List())++a) ++ acc
          case _                        => throw ThisIsImpossible
        }

        /* ============ Term ============ */
        case Term.Block(stats) =>
          stats.filter(isInteresting).foldLeft(List[(Type.Name, Term.Name, Term.Name,
                                                     Option[Type], Option[Term])]())((a, elem) =>
            inner(elem, currentClass, currentMethod, List())++a) ++ acc

        case Term.If(_, thenBranch, elseBranch) => {
          val collectedFromThen = if (isInteresting(thenBranch))
                                    inner(thenBranch, currentClass, currentMethod, List())
                                  else List()
          val collectedFromElse = if (isInteresting(elseBranch))
                                    inner(elseBranch, currentClass, currentMethod, List())
                                  else List()
          collectedFromThen ++ collectedFromElse ++ acc
        }

        case Term.Try(tryStat, catchCases, finallyOpt) => finallyOpt match {
          case Some(finallyStat) => {
            val collectedFromTry = inner(tryStat, currentClass, currentMethod, List())
            val collectedFromFinally = inner(finallyStat, currentClass, currentMethod, List())
            collectedFromTry ++ collectedFromFinally ++ acc
          }
          case None              => {
            val collectedFromTry = inner(tryStat, currentClass, currentMethod, List())
            collectedFromTry ++ acc
          }
        }

        case Term.While(_, body) =>
          inner(body, currentClass, currentMethod, acc)

        case Term.For(_, body) =>
          inner(body, currentClass, currentMethod, acc)

        /* ============ Source ============ */
        case Source(stats) =>
          stats.filter(isInteresting).foldLeft(List[(Type.Name, Term.Name, Term.Name,
                                                     Option[Type], Option[Term])]())((a, elem) =>
            inner(elem, currentClass, currentMethod, List())++a) ++ acc

        /* ============ Pkg ============ */
        case Pkg(_, stats) =>
          stats.filter(isInteresting).foldLeft(List[(Type.Name, Term.Name, Term.Name,
                                                     Option[Type], Option[Term])]())((a, elem) =>
            inner(elem, currentClass, currentMethod, List())++a) ++ acc

        case otherwise => {
          println(otherwise);
          throw new MatchError(otherwise.productPrefix)
        }
      }

    inner(tree, Type.Name("ph"), Term.Name("ph"), List())
  }


  def scopeGreaterThanClass(tree: Tree): Boolean = tree match {
    case Defn.Class(_) => true
    case Source(_)     => true
    case Pkg(_)        => true
    case _             => false
  }


  def innerClassChainCollector(tree: Tree): List[(Type.Name, Type.Name)] = {
    def inner(tree: Tree, currentClass: Type.Name,
              acc: List[(Type.Name, Type.Name)]):
        List[(Type.Name, Type.Name)] = tree match {
      /* ============ Class ============ */
      case Defn.Class(_, newClassName, _, _, Template(_, _, _, body)) =>
        (currentClass, newClassName)::body.foldLeft(List[(Type.Name, Type.Name)]())((a, elem) =>
          inner(elem, newClassName, List())++a) ++ acc

      /* ============ Source ============ */
      case Source(stats) =>
        stats.filter(scopeGreaterThanClass).foldLeft(List[(Type.Name, Type.Name)]())((a, elem) =>
          inner(elem, currentClass, List())++a) ++ acc

      /* ============ Pkg ============ */
      case Pkg(_, stats) =>
        stats.filter(scopeGreaterThanClass).foldLeft(List[(Type.Name, Type.Name)]())((a, elem) =>
          inner(elem, currentClass, List())++a) ++ acc

      case _ => acc
    }

    inner(tree, Type.Name("ph"), List())
  }


  def treeIsInTreeList(tree: Tree, treeList: List[Tree]): Boolean =
    treeList.foldLeft[Boolean](false)((acc, treeElem) =>
      tree.isEqual(treeElem) || acc)


  /**
    * Collects the class names and identifiers of the methods that
    * refers to the given vars.
    *
    * @param tree the tree to search
    * @param vars the vars in question
    * @return list of pairs comprised of class names and method names
    */
  def referringMethodCollector(tree: Tree,
                               vars: List[(Type.Name, Term.Name, Term.Name,
                                           Option[Type], Option[Term])]):
      List[(Type.Name, Term.Name)] = {
    val globalScopeVarNamesOnly: List[Term.Name] = vars.filter(varTup =>
      varTup._2.isEqual(Term.Name("ph"))).map(_._3)

    println("looking for these: "+ globalScopeVarNamesOnly)

    def inner(tree: Tree,
              currentClass: Type.Name,
              currentMethod: Term.Name,
              acc: List[(Type.Name, Term.Name)]):
        List[(Type.Name, Term.Name)] = {

      tree match {
        case _: Lit => acc

        case _: Import => acc

        case Pat.Var(varName) =>
          if (treeIsInTreeList(varName, globalScopeVarNamesOnly))
            (currentClass, currentMethod)::acc
          else
            acc

        case Defn.Val(_, _, _, term) =>
          if (treeIsInTreeList(term, globalScopeVarNamesOnly))
            (currentClass, currentMethod)::acc
          else
            acc

        case Defn.Var(_, _, _, termOpt) =>
          termOpt match {
            case None       => acc
            case Some(term) => inner(term, currentClass, currentMethod, acc)
          }

        case defn @ Defn.Def(_, newMethodName, _, _, _, stat)
            if isDefun(defn) =>
          inner(stat, currentClass, newMethodName, acc)

        case termName: Term.Name =>
          if (treeIsInTreeList(termName, globalScopeVarNamesOnly))
            (currentClass, currentMethod)::acc
          else
            acc

        case Term.Select(term, name) =>
          inner(term, currentClass, currentMethod, acc)

        case Term.Apply(_, args) => {
          val found = args.foldLeft(false)((acc, arg) =>
            acc || treeIsInTreeList(arg, globalScopeVarNamesOnly))
          if (found)
            (currentClass, currentMethod)::acc
          else
            acc
        }

        case Term.ApplyUsing(_, args) => {
          val found = args.foldLeft(false)((acc, arg) =>
            acc || treeIsInTreeList(arg, globalScopeVarNamesOnly))
          if (found)
            (currentClass, currentMethod)::acc
          else acc
        }

        case Term.ApplyInfix(lhs, _, _, args) => {
          val argFound = args.foldLeft(false)((acc, arg) =>
            acc || treeIsInTreeList(arg, globalScopeVarNamesOnly))
          val lhsFound = treeIsInTreeList(lhs, globalScopeVarNamesOnly);
          (argFound, lhsFound) match {
            case (false, false) => acc
            case _ => (currentClass, currentMethod)::acc
          }
        }

        case Term.ApplyUnary(_, arg) =>
          if (treeIsInTreeList(arg, globalScopeVarNamesOnly))
            (currentClass, currentMethod)::acc
          else acc

        case Term.Assign(lhs, rhs) =>
          if (treeIsInTreeList(lhs, globalScopeVarNamesOnly) ||
                treeIsInTreeList(rhs, globalScopeVarNamesOnly))
            (currentClass, currentMethod)::acc
          else acc

        case Term.Return(expr) =>
          if (treeIsInTreeList(expr, globalScopeVarNamesOnly))
            (currentClass, currentMethod)::acc
          else acc

        case Term.Block(stats) =>
          stats.foldLeft(List[(Type.Name, Term.Name)]())((a, stat) =>
            inner(stat, currentClass, currentMethod, List())++a) ++ acc

        case Term.If(cond, thenBranch, elseBranch) => {
          val emptyList = List[(Type.Name, Term.Name)]()
          val collectedFromCond = inner(cond, currentClass,
                                        currentMethod, emptyList)
          val collectedFromThen = inner(thenBranch,
                                        currentClass, currentMethod, emptyList)
          val collectedFromElse = inner(elseBranch, currentClass,
                                        currentMethod, emptyList)
          collectedFromCond ++ collectedFromThen ++
            collectedFromElse ++ acc
        }

        case Term.Try(expr, _, finallyStats) => {
          val emptyList = List[(Type.Name, Term.Name)]()
          val collectedFromTry = inner(expr, currentClass, currentMethod, emptyList)
          val collectedFromFinally = finallyStats match {
            case None       => List()
            case Some(expr) => inner(expr, currentClass, currentMethod, emptyList)
          }
          collectedFromTry ++ collectedFromFinally ++ acc
        }

        case Term.While(cond, body) => {
          val emptyList = List[(Type.Name, Term.Name)]()
          val collectedFromCond = inner(cond, currentClass, currentMethod, emptyList)
          val collectedFromBody = inner(body, currentClass, currentMethod, emptyList)
          collectedFromCond ++ collectedFromBody ++ acc
        }

        case Term.For(_, body) => {
          val emptyList = List[(Type.Name, Term.Name)]()
          inner(body, currentClass, currentMethod, emptyList) ++ acc
        }

        case Term.Throw(expr) => {
          val emptyList = List[(Type.Name, Term.Name)]()
          inner(expr, currentClass, currentMethod, emptyList)
        }

        case Term.New(Init(_, _, argss)) =>
          argss.map(_.foldLeft(List[(Type.Name, Term.Name)]())((a, stat) =>
                      inner(stat, currentClass, currentMethod, List())++a))
            .foldLeft(List[(Type.Name, Term.Name)]())((a, elem) =>
              elem ++ a)

        case Defn.Class(_, newClassName, _, _, Template(_, _, _, stats)) =>
          stats.foldLeft(List[(Type.Name, Term.Name)]())((a, elem) =>
            inner(elem, newClassName, currentMethod, List())++a) ++ acc

        case Defn.Object(_, Term.Name(newClassNameString), Template(_, _, _, stats)) =>
          stats.foldLeft(List[(Type.Name, Term.Name)]())((a, elem) =>
            inner(elem, Type.Name(newClassNameString), currentMethod, List())++a) ++ acc

        case Source(stats) =>
          stats.foldLeft(List[(Type.Name, Term.Name)]())((a, elem) =>
            inner(elem, currentClass, currentMethod, List())++a) ++ acc

        case Pkg(_, stats) =>
          stats.foldLeft(List[(Type.Name, Term.Name)]())((a, elem) =>
            inner(elem, currentClass, currentMethod, List())++a) ++ acc

        case otherwise => {
          println("You missed (referringMethodCollector/inner): " + otherwise +
                    " which is: " + otherwise.productPrefix)
          acc
        }
      }
    }
    inner(tree, Type.Name("ph"), Term.Name("ph"), List());
  }

  /**
    * Looks for all callers of the given callee.
    *
    * @param tree the tree in which to look for callers.
    * @param callee the callee in question.
    */
  def callerCollector(tree: Tree, callee: (Type.Name, Term.Name)) = {
    def inner(tree: Tree, acc: List[(Type.Name, Term.Name)]) = tree match {
      /* ============ atoms ============ */

      case _: Lit => throw TODO

      case Pat.Var(varName) => throw TODO

      case termName: Term.Name => throw TODO

      /* ============ statements ============ */

      case Defn.Val(_, _, _, term) => throw TODO

      case Defn.Var(_, _, _, termOpt) => throw TODO

      case Term.Select(term, name) => throw TODO

      case Term.Apply(_, args) => throw TODO

      case Term.ApplyUsing(_, args) => throw TODO

      case Term.ApplyInfix(lhs, _, _, args) => throw TODO

      case Term.ApplyUnary(_, arg) => throw TODO

      case Term.Assign(lhs, rhs) => throw TODO

      case Term.Return(expr) => throw TODO

      case Term.New(Init(_, _, argss)) => throw TODO

      /* ============ Bigger statements ============ */

      case Term.Block(stats) => throw TODO

      case Term.If(cond, thenBranch, elseBranch) => throw TODO

      case Term.Try(expr, _, finallyStats) => throw TODO

      case Term.While(cond, body) => throw TODO

      case Term.For(_, body) => throw TODO

      case Term.Throw(expr) => throw TODO

      /* ============ Beyond statement levels ============ */

      case defn @ Defn.Def(_, newMethodName, _, _, _, stat) if isDefun(defn) => throw TODO

      case Defn.Class(_, newClassName, _, _, Template(_, _, _, stats)) => throw TODO

      case Defn.Object(_, Term.Name(newClassNameString), Template(_, _, _, stats)) => throw TODO

      case Source(stats) => throw TODO

      case Pkg(_, stats) => throw TODO

      case _: Import => throw TODO

      case otherwise => {
        println(s"""You missed (referringMethodCollector/inner): $otherwise
                  which is: ${otherwise.productPrefix}""")
        acc
      }
    }
  }
}
