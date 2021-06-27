package kr.ac.korea.prl.stateless.TreeTraverser

import scala.annotation.tailrec
import scala.meta._
import scala.meta.contrib._
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import scala.math.Ordering.Int
import scala.util.{Try, Success, Failure}

import org.jgrapht.traverse.BreadthFirstIterator
import org.jgrapht.graph.DefaultEdge

import kr.ac.korea.prl.stateless.TreeGraph._
import kr.ac.korea.prl.stateless.CustomTree._
import org.jgrapht.graph.DirectedAcyclicGraph

case object ThisIsImpossible extends Exception
case object EmptyInputList extends Exception

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
        while (innerIterator.hasNext) {
          val smoltree = innerIterator.next()
          if (smoltree.isInstanceOf[DefVar]) {
            val defVar = smoltree.asInstanceOf[DefVar]
            val myClasses = findMyClasses(smoltree, treeGraph)
            val myMethods = findMyMethods(smoltree, treeGraph)
            val myName = defVar.pats.head.asInstanceOf[PatVar].name.asInstanceOf[TermName]
            val myTypeName = defVar.decltpe
            val myDefVal = defVar.rhs
            val allCombinations = for {
              myClass <- myClasses
              myMethod <- myMethods
            } yield (myClass, myMethod, myName, myTypeName, myDefVal)
            allCombinations.foreach(list += _)
          }
        }
      }
    }
    list.toList.distinct
  }


  def innerClassChainCollector(tree: CustomTree): List[(TypeName, TypeName)] = {

    def inner(tree: CustomTree, currentClass: TypeName,
              acc: List[(TypeName, TypeName)]):
        List[(TypeName, TypeName)] = tree match {

      /* ============ Class ============ */
      case DefClass(_, newClassName, _, _, CustomTemplate(_, _, _, body)) =>
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

      case otherwise => acc
    }

    inner(tree, TypeName("toplevel"), List())
  }


  /**
    * Collects the class names and identifiers of the methods that
    * refers to the given vars.
    *
    * @param tree the tree to search
    * @param vars the vars in question
    * @return list of pairs comprised of class names and method names
    */
  def referringMethodCollector(customTree: CustomTree,
                               vars: List[(TypeName, TermName, TermName,
                                           Option[CustomType], Option[CustomTerm])]):
      List[(TypeName, TermName)] = {

    val globalScopeVarNamesOnly: List[TermName] = vars.filter(varTup =>
      varTup._2.equals(TermName("ph"))).map(_._3)

    val treeGraph = TreeGraph.graphFromCustomTree(customTree)
    val iterator = new BreadthFirstIterator[CustomTree, DefaultEdge](treeGraph)
    val list = ListBuffer[(TypeName, TermName)]()

    while (iterator.hasNext()) {
      val elem = iterator.next()
      if (isDefun(elem)) {
        // spawn another BFS
        val innerIterator = new BreadthFirstIterator(TreeGraph.graphFromCustomTree(elem))
        while (innerIterator.hasNext()) {
          // catching Patterns
          val smoltree = innerIterator.next()
          if (smoltree.isInstanceOf[PatVar]) {
            val smoltreeVarName = smoltree.asInstanceOf[PatVar].name
            if (globalScopeVarNamesOnly.contains(smoltreeVarName)) {
              val allCombinations = for {
                myClass <- findMyClasses(smoltree, treeGraph)
                myMeth <- findMyMethods(smoltree, treeGraph)
              } yield (myClass, myMeth)
              allCombinations.foreach(list += _)
            }
          }

          // catching generic Term Names
          if (smoltree.isInstanceOf[TermName]) {
            val smoltreeVarName = smoltree.asInstanceOf[TermName]
            if (globalScopeVarNamesOnly.contains(smoltreeVarName)) {
              val allCombinations = for {
                myClass <- findMyClasses(smoltree, treeGraph)
                myMeth <- findMyMethods(smoltree, treeGraph)
              } yield (myClass, myMeth)
              allCombinations.foreach(list += _)
            }
          }
        }
      }
    }
    list.toList.distinct
  }


  /**
    * Find the key with maximum value in a list of tuples (alist).
    *
    * @param alist: Tuple list where the second element is an integer.
    * @return the max key with the maximum value.
    */
  def findKeysWithMaxVal(alist: List[(CustomTree, Int)]):
  Try[List[CustomTree]] = Try {
    if (alist.isEmpty)
      throw EmptyInputList
    val vals = alist.map(_._2)
    val maxVal = vals.max
    alist.filter(tup => tup._2 == maxVal).map(_._1)
  }


  def classOrObjectCaster(ctree: CustomTree):
      Either[DefClass, DefObject] = {
    try {
      Left(ctree.asInstanceOf[DefClass])
    } catch {
      case _: ClassCastException =>
        Right(ctree.asInstanceOf[DefObject])
    }
  }


  def extractTypeNameFromClassOrObject(ctree: CustomTree): TypeName = {
    try {
      ctree.asInstanceOf[DefClass].name
    } catch {
      case _: ClassCastException =>
        TypeName(ctree.asInstanceOf[DefObject].name.value)
    }
  }


  def findMyClasses(elem: CustomTree,
                    treeGraph: DirectedAcyclicGraph[CustomTree, DefaultEdge]):
      List[TypeName] = {
    // find elem's predecessor in treeGraph whose class is defClass or defObject
    val classes = treeGraph.getAncestors(elem).asScala.toList.filter(isClassDef)

    // now, get the most specific defClass: because defClass can be nested
    val bfsIterator = new BreadthFirstIterator[CustomTree, DefaultEdge](treeGraph)

    // first, drain out the iterator
    while (bfsIterator.hasNext) { val _ = bfsIterator.next() }

    val classesAndDepth = classes.map(klass =>
      (klass, bfsIterator.getDepth(klass))
    )

    // get the maximum class in terms of depth
    return findKeysWithMaxVal(classesAndDepth) match {
      case Success(succResult) => succResult.map(extractTypeNameFromClassOrObject(_))
      case Failure(failResult) => {
        println(s"elem: ${elem.toString}")
        throw EmptyInputList
      }
    }
  }


  def findMyMethods(elem: CustomTree,
                    treeGraph: DirectedAcyclicGraph[CustomTree, DefaultEdge]):
      List[TermName] = {

    // find elem's predecessor in treeGraph which is a defun
    val methods = treeGraph.getAncestors(elem).asScala.toList.filter(isDefun)

    // now, get the most specific defun: because defuns can be nested
    val bfsIterator = new BreadthFirstIterator[CustomTree, DefaultEdge](treeGraph)

    // first, drain out the iterator
    while (bfsIterator.hasNext) { val _ = bfsIterator.next() }

    val methodsAndDepth = methods.map(meth =>
      (meth, bfsIterator.getDepth(meth))
    )

    // get the maximum meth in terms of depth
    return findKeysWithMaxVal(methodsAndDepth) match {
      case Success(succResult) => succResult.map(_.asInstanceOf[DefDef].name)
      case Failure(failResult) => failResult match {
        case EmptyInputList => List(TermName("ph"))
      }
    }
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
    val targetCalleeClass = callee._1
    val targetCalleeName = callee._2

    while (iterator.hasNext) {
      val elem = iterator.next()
      if (isDefun(elem)) {
        // spawn another BFS
        val innerIterator = new BreadthFirstIterator(TreeGraph.graphFromCustomTree(elem))
        // look for calls
        while (innerIterator.hasNext) {
          val smoltree = innerIterator.next()
          if (smoltree.isInstanceOf[TermApply]) {
            val methodClassMatches = findMyClasses(smoltree, TreeGraph.graphFromCustomTree(customTree))
              .equals(callee._1)
            val methodIdentifierMatches = smoltree.asInstanceOf[TermApply].fun match {
              case target: TermName => target.asInstanceOf[TermName] equals callee._2
              case target: TermSelect => target.name equals callee._2
            }
            if (methodClassMatches && methodIdentifierMatches)
              findMyClasses(smoltree, TreeGraph.graphFromCustomTree(customTree)).foreach(
                myClass => list.+=((myClass, smoltree.asInstanceOf[TermApply].fun.asInstanceOf[TermName]))
              )
          }
        }
      }
    }
    list.toList.distinct
  }
}
