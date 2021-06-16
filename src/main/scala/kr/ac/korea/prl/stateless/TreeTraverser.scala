package kr.ac.korea.prl.stateless.TreeTraverser

import scala.annotation.tailrec
import scala.meta._
import scala.meta.contrib._
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import scala.math.Ordering.Int

import java.io.InvalidClassException

import org.jgrapht.traverse.BreadthFirstIterator
import org.jgrapht.graph.DefaultEdge

import kr.ac.korea.prl.stateless.TreeGraph._
import kr.ac.korea.prl.stateless.CustomTree._
import org.jgrapht.graph.DirectedAcyclicGraph

object ThisIsImpossible extends Exception

object TreeTraverser {

  def truncatedPrint(tree: CustomTree): String = {
    val string = tree.toString()
    string.slice(0, 15)+"..."
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

  def isDefun(tree: CustomTree): Boolean = tree match {
    case DefDef(_, _, _, paramList, _, _) => !paramList.isEmpty
    case _                                => false
  }

  def isClassDef(tree: CustomTree): Boolean = tree match {
    case _: DefClass  => true
    case _: DefObject => true
    case _            => false
  }


  def scopeGreaterThanClass(tree: CustomTree): Boolean = tree match {
    case _: DefClass      => true
    case _: CustomSource  => true
    case _: CustomPkg     => true
    case _                => false
  }

  /**
    * Collect all the vars in the given tree.
    * NOTE: This function is deprecated as scala.meta.Tree is no longer used directly.
    *
    * @param tree
    * @return List of triples: ClassID * MethodID * VarID * TypeOption * TermOption
    */
  def varCollector(tree: Tree):
      List[(Type.Name, Term.Name, Term.Name,
            Option[Type], Option[Term])] = {

    def isInteresting(elem: Stat) = elem match {
      /* ============ Decl ============ */
      case _: Decl.Var => true
      /* ============ Defn ============ */
      case _: Defn.Var => true
      case _: Defn.Def => true
      case _: Defn.Class => true
      case _: Defn.Trait => true
      case _: Defn.Object => true
      /* ============ Term ============ */
      case _: Term.Block => true
      case _: Term.If => true
      case _: Term.Try => true
      case _: Term.While => true
      case _: Term.For => true
      /* ============ Source ============ */
      case _: Source => true
      case _: Pkg => true

      case otherwise => false
    }

    def functionBodyIsComposite(statPossiblyBlock: Term): Boolean =
      statPossiblyBlock match {
        case _: Term.Block => true
        case _: Term.If    => true
        case _: Term.Try   => true
        case _: Term.While => true
        case _: Term.For   => true
        case _            => false
      }

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


  /**
    * Collects all the vars defined in the given piece of program.
    *
    * @param tree must be of CustomTree type
    * @return The list of the 5-tuple:
    *   1. The var's class name
    *   2. The var's method name ("ph" if none)
    *   3. The var's name
    *   4. The var's type name (Option)
    *   5. The var's definition value (Option)
    */
  def varCollector(tree: CustomTree): List[(TypeName, TermName,
                                            TermName,
                                            Option[CustomType],
                                            Option[CustomTerm])] = {
    def isInteresting(elem: CustomTree): Boolean =
      elem match {
        /* ============ Decl ============ */
        case _: DeclVar      => true
        /* ============ Defn ============ */
        case _: DefVar       => true
        case _: DefDef       => true
        case _: DefClass     => true
        case _: DefTrait     => true
        case _: DefObject    => true
        /* ============ Term ============ */
        case _: TermBlock    => true
        case _: TermIf       => true
        case _: TermTry      => true
        case _: TermWhile    => true
        case _: TermFor      => true
        /* ============ Source ============ */
        case _: CustomSource => true
        case _: CustomPkg    => true

        case otherwise       => false
      }

    def functionBodyIsComposite(statPossiblyBlock: CustomTerm): Boolean =
      statPossiblyBlock match {
        case _: TermBlock => true
        case _: TermIf    => true
        case _: TermTry   => true
        case _: TermWhile => true
        case _: TermFor   => true
        case _            => false
      }

    val treeGraph = TreeGraph.graphFromCustomTree(tree)
    val iterator = new BreadthFirstIterator(treeGraph)
    val list = ListBuffer[(TypeName, TermName,
                           TermName, Option[CustomType],
                           Option[CustomTerm])]()

    while (iterator.hasNext) {
      val elem = iterator.next()
      if (isInteresting(elem)) {
        // spawn another BFS
        val innerIterator = new BreadthFirstIterator(TreeGraph.graphFromCustomTree(elem))
        while (iterator.hasNext) {
          val smoltree = innerIterator.next()
          if (smoltree.isInstanceOf[DefVar]) {
            val defVar = smoltree.asInstanceOf[DefVar]
            val myClass = findMyClass(smoltree, treeGraph)
            val myMeth = findMyMethod(smoltree, treeGraph)
            val myName = defVar.pats.head.asInstanceOf[TermName]
            val myTypeName = defVar.decltpe
            val myDefVal = defVar.rhs
            list.+=((myClass, myMeth, myName, myTypeName, myDefVal))
          }
        }
      }
    }
    list.toList
  }


  def innerClassChainCollector(tree: CustomTree): List[(TypeName, TypeName)] = {

    def inner(tree: CustomTree, currentClass: TypeName,
              acc: List[(TypeName, TypeName)]):
        List[(TypeName, TypeName)] = tree match {

      /* ============ Class ============ */
      case DefClass(_, newClassName, _, _, Template(_, _, _, body)) =>
        (currentClass, newClassName)::body.foldLeft(List[(TypeName, TypeName)]())((a, elem) =>
          inner(elem.asInstanceOf[CustomTree], newClassName, List())++a) ++ acc

      /* ============ Source ============ */
      case CustomSource(stats) =>
        stats.filter(scopeGreaterThanClass).foldLeft(List[(TypeName, TypeName)]())((a, elem) =>
          inner(elem.asInstanceOf[CustomTree], currentClass, List())++a) ++ acc

      /* ============ Pkg ============ */
      case CustomPkg(_, stats) =>
        stats.filter(scopeGreaterThanClass).foldLeft(List[(TypeName, TypeName)]())((a, elem) =>
          inner(elem.asInstanceOf[CustomTree], currentClass, List())++a) ++ acc

      case _ => acc
    }

    inner(tree, TypeName("ph"), List())
  }


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

    def treeIsInTreeList(tree: Tree, treeList: List[Tree]): Boolean =
      treeList.foldLeft[Boolean](false)((acc, treeElem) =>
        tree.isEqual(treeElem) || acc)

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

        case Term.Interpolate(prefix, parts, args) => {
          val found = args.foldLeft(false)((acc, arg) =>
            acc || treeIsInTreeList(arg, globalScopeVarNamesOnly))
          if (found)
            (currentClass, currentMethod)::acc
          else
            acc
        }

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
          println(s"""You missed (referringMethodCollector/inner): ${otherwise} which is: ${otherwise.productPrefix} and its structure is ${otherwise.structure}""")
          acc
        }
      }
    }
    inner(tree, Type.Name("ph"), Term.Name("ph"), List());
  }


  /**
    * Find the key with maximum value in a list of tuples (alist).
    *
    * @param alist: Tuple list where the second element is an integer.
    * @return the max key with the maximum value.
    */
  def findKeyWithMaxVal[A, Int](alist: List[(A, Int)]): A = {

    implicit def orderingInt[Int]: Ordering[Int] =
      Ordering.by(identity)

    val vals = alist.map(_._2)
    val maxVal = vals.max(orderingInt[Int])
    val cellsWithMaxVal = alist.filter(tup => tup._2 == maxVal)
    assert(cellsWithMaxVal.length == 1)
    cellsWithMaxVal.head._1
  }


  def findMyClass(elem: CustomTree,
                  treeGraph: DirectedAcyclicGraph[CustomTree, DefaultEdge]):
      TypeName = {
    // find elem's predecessor in treeGraph whose class is defClass or defObject
    val classes = treeGraph.getAncestors(elem)
      .asScala
      .toList
      .filter(isClassDef)

    // now, get the most specific defClass: because defClass can be nested
    val bfsIterator = new BreadthFirstIterator[CustomTree, DefaultEdge](treeGraph)

    val classesAndDepth = classes.map(klass =>
      (klass, bfsIterator.getDepth(klass))
    )

    // get the maximum class in terms of depth
    findKeyWithMaxVal(classesAndDepth).asInstanceOf[TypeName]
  }


  def findMyMethod(elem: CustomTree,
                   treeGraph: DirectedAcyclicGraph[CustomTree, DefaultEdge]):
      TermName = {

    // find elem's predecessor in treeGraph which is a defun
    val methods = treeGraph.getAncestors(elem)
      .asScala
      .toList
      .filter(isDefun)

    // now, get the most specific defun: because defuns can be nested


    ???
  }


  /**
    * Looks for all callers of the given callee.
    *
    * @param tree the tree in which to look for callers.
    * @param callee the callee in question.
    */
  def callerCollector(customTree: CustomTree, callee: (TypeName, TermName)):
      List[(TypeName, TermName)] = {
    val treeGraph = TreeGraph.graphFromCustomTree(customTree)
    val iterator = new BreadthFirstIterator(treeGraph)
    val list = ListBuffer[(TypeName, TermName)]()

    while (iterator.hasNext) {
      val elem = iterator.next()
      if (isDefun(elem)) {
        // spawn another BFS
        val innerIterator = new BreadthFirstIterator(TreeGraph.graphFromCustomTree(elem))
        // look for calls
        while (innerIterator.hasNext) {
          val smoltree = innerIterator.next()
          if (smoltree.isInstanceOf[TermApply]) {
            val methodIdentifierMatches = smoltree.asInstanceOf[TermApply]
              .fun
              .asInstanceOf[TermName]
              .equals(customTree.asInstanceOf[TermName])
            val methodClassMatches = findMyClass(smoltree, TreeGraph.graphFromCustomTree(customTree)).
              equals(customTree.asInstanceOf[TypeName])
            if (methodIdentifierMatches && methodClassMatches)
              list.+=((findMyClass(smoltree, TreeGraph.graphFromCustomTree(customTree)),
                       smoltree.asInstanceOf[TermName]))
          }
        }
      }
    }
    list.toList
  }
}
