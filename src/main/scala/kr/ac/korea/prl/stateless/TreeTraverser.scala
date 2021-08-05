package kr.ac.korea.prl.stateless.TreeTraverser

import scala.annotation.tailrec
import scala.meta._
import scala.meta.contrib._
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import scala.math.Ordering.Int

import org.jgrapht.traverse.BreadthFirstIterator
import org.jgrapht.graph.DefaultEdge

import kr.ac.korea.prl.stateless.TreeGraph._
import kr.ac.korea.prl.stateless.CustomTree._
import kr.ac.korea.prl.stateless.CustomTree.Predicates._
import kr.ac.korea.prl.stateless.Utils.Utils._
import org.jgrapht.graph.DirectedAcyclicGraph

case object ThisIsImpossible extends Exception
case class InvalidInput(content: String) extends Exception

object TreeTraverser {

  type TreeGraph = DirectedAcyclicGraph[CustomTree, DefaultEdge]
  type BFS = BreadthFirstIterator[CustomTree, DefaultEdge]

  def truncatedString(tree: CustomTree): String = {
    val string = tree.toString()
    string.slice(0, 15) + "..."
  }

  def scopeGreaterThanClass(tree: CustomTree): Boolean = tree match {
    case _: DefClass     => true
    case _: CustomSource => true
    case _: CustomPkg    => true
    case _               => false
  }

  /** Collects all the vars defined in the given piece of program.
    *
    * @param tree must be of CustomTree type
    * @return The list of the 5-tuple:
    *   1. The var's class name
    *   2. The var's method name ("ph" if none)
    *   3. The var's name
    *   4. The var's type name (Option)
    *   5. The var's definition value (Option)
    */
  def varCollector(tree: CustomTree): List[
    (TypeName, TermName, TermName, Option[CustomType], Option[CustomTerm])
  ] = {
    def isInteresting(elem: CustomTree): Boolean =
      elem match {
        /* ============ Decl ============ */
        case _: DeclVar => true
        /* ============ Defn ============ */
        case _: DefVar    => true
        case _: DefDef    => true
        case _: DefClass  => true
        case _: DefTrait  => true
        case _: DefObject => true
        /* ============ Term ============ */
        case _: TermBlock => true
        case _: TermIf    => true
        case _: TermTry   => true
        case _: TermWhile => true
        case _: TermFor   => true
        /* ============ Source ============ */
        case _: CustomSource => true
        case _: CustomPkg    => true

        case otherwise => false
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
    val iterator = new BFS(treeGraph)
    val list = ListBuffer[
      (TypeName, TermName, TermName, Option[CustomType], Option[CustomTerm])
    ]()

    while (iterator.hasNext) {
      val elem = iterator.next()
      if (isInteresting(elem)) {
        // spawn another BFS
        val innerIterator = new BFS(TreeGraph.graphFromCustomTree(elem))
        while (innerIterator.hasNext) {
          val smoltree = innerIterator.next()
          if (smoltree.isInstanceOf[DefVar]) {
            val defVar = smoltree.asInstanceOf[DefVar]
            val myClasses = findMyClasses(smoltree, treeGraph)
            val myMethods = findMyMethods(smoltree, treeGraph)
            val myName =
              defVar.pats.head.asInstanceOf[PatVar].name.asInstanceOf[TermName]
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

    def inner(
        tree: CustomTree,
        currentClass: TypeName,
        acc: List[(TypeName, TypeName)]
    ): List[(TypeName, TypeName)] = tree match {

      /* ============ Class ============ */
      case DefClass(_, newClassName, _, _, CustomTemplate(_, _, _, body)) =>
        (currentClass, newClassName) :: body.foldLeft(
          List[(TypeName, TypeName)]()
        )((a, elem) =>
          inner(elem.asInstanceOf[CustomTree], newClassName, List()) ++ a
        ) ++ acc

      /* ============ Source ============ */
      case CustomSource(stats) =>
        stats
          .filter(scopeGreaterThanClass)
          .foldLeft(List[(TypeName, TypeName)]())((a, elem) =>
            inner(elem.asInstanceOf[CustomTree], currentClass, List()) ++ a
          ) ++ acc

      /* ============ Pkg ============ */
      case CustomPkg(_, stats) =>
        stats
          .filter(scopeGreaterThanClass)
          .foldLeft(List[(TypeName, TypeName)]())((a, elem) =>
            inner(elem.asInstanceOf[CustomTree], currentClass, List()) ++ a
          ) ++ acc

      case otherwise => acc
    }

    inner(tree, TypeName("toplevel"), List())
  }

  /** Collects the class names and identifiers of the methods that
    * refers to the given vars.
    *
    * @param tree the tree to search
    * @param vars the vars in question
    * @return list of pairs comprised of class names and method names
    */
  def referringMethodCollector(
      customTree: CustomTree,
      vars: List[
        (TypeName, TermName, TermName, Option[CustomType], Option[CustomTerm])
      ]
  ): List[(TypeName, TermName)] = {

    val globalScopeVarNamesOnly: List[TermName] =
      vars.filter(varTup => varTup._2.equals(TermName("ph"))).map(_._3)

    val treeGraph = TreeGraph.graphFromCustomTree(customTree)
    val iterator = new BFS(treeGraph)
    val list = ListBuffer[(TypeName, TermName)]()

    while (iterator.hasNext()) {
      val elem = iterator.next()
      if (isDefun(elem)) {
        // spawn another BFS
        val innerIterator = new BFS(TreeGraph.graphFromCustomTree(elem))
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

  def classOrObjectCaster(ctree: CustomTree): Either[DefClass, DefObject] = {
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

  def findMyClasses(elem: CustomTree, treeGraph: TreeGraph): List[TypeName] = {
    // find elem's predecessor in treeGraph whose class is defClass or defObject
    val classes = treeGraph.getAncestors(elem).asScala.toList.filter(isClassDef)

    // now, get the most specific defClass: because defClass can be nested
    val bfsIterator = new BFS(treeGraph)

    // first, drain out the iterator
    while (bfsIterator.hasNext) { val _ = bfsIterator.next() }

    val classesAndDepth =
      classes.map(klass => (klass, bfsIterator.getDepth(klass)))

    // get the maximum class in terms of depth
    try {
      val result = findKeysWithMaxVal(classesAndDepth)
      result.map(extractTypeNameFromClassOrObject)
    } catch {
      case _: EmptyInputList => {
        throw EmptyInputList(s"elem: ${elem.toString}")
      }
    }
  }

  def findMyMethods(elem: CustomTree, treeGraph: TreeGraph): List[TermName] = {

    // find elem's predecessor in treeGraph which is a defun
    val methods = treeGraph.getAncestors(elem).asScala.toList.filter(isDefun)

    // now, get the most specific defun: because defuns can be nested
    val bfsIterator = new BFS(treeGraph)

    // first, drain out the iterator
    while (bfsIterator.hasNext) { val _ = bfsIterator.next() }

    val methodsAndDepth =
      methods.map(meth => (meth, bfsIterator.getDepth(meth)))

    try {
      val result = findKeysWithMaxVal(methodsAndDepth)
      result.map(_.asInstanceOf[DefDef].name)
    } catch {
      case _: EmptyInputList => throw EmptyInputList(s"elem: ${elem.toString}")
    }
  }

  /** Looks for all callers of the given callee.
    *
    * @param tree the tree in which to look for callers.
    * @param callee the callee in question.
    */
  def callerCollector(
      customTree: CustomTree,
      callee: (TypeName, TermName)
  ): List[(TypeName, TermName)] = {
    val treeGraph = TreeGraph.graphFromCustomTree(customTree)
    val iterator = new BFS(treeGraph)
    val list = ListBuffer[(TypeName, TermName)]()
    val targetCalleeClass = callee._1
    val targetCalleeName = callee._2

    while (iterator.hasNext) {
      val elem = iterator.next()
      if (isDefun(elem)) {
        // spawn another BFS
        val innerIterator = new BFS(TreeGraph.graphFromCustomTree(elem))
        // look for calls
        while (innerIterator.hasNext) {
          val smoltree = innerIterator.next()
          if (smoltree.isInstanceOf[TermApply]) {
            val methodClassMatches =
              findMyClasses(smoltree, TreeGraph.graphFromCustomTree(customTree))
                .equals(callee._1)
            val methodIdentifierMatches =
              smoltree.asInstanceOf[TermApply].fun match {
                case target: TermName =>
                  target.asInstanceOf[TermName] equals callee._2
                case target: TermSelect => target.name equals callee._2
                case _                  => throw ThisIsImpossible
              }
            if (methodClassMatches && methodIdentifierMatches)
              findMyClasses(smoltree, TreeGraph.graphFromCustomTree(customTree))
                .foreach(myClass =>
                  list.+=(
                    (
                      myClass,
                      smoltree
                        .asInstanceOf[TermApply]
                        .fun
                        .asInstanceOf[TermName]
                    )
                  )
                )
          }
        }
      }
    }
    list.toList.distinct
  }

  /** @param customTree
    * @param caller
    * @return
    */
  def calleeCollector(
      customTree: CustomTree,
      caller: (TypeName, TermName)
  ): List[(TypeName, TermName)] = {
    val treeGraph = TreeGraph.graphFromCustomTree(customTree)
    val targetCalleeClass = caller._1
    val targetCalleeName = caller._2

    val iterator = new BFS(treeGraph)

    var callerTree =
      LitNull().asInstanceOf[CustomTree] // Just a placeholder definition

    // first, spot the caller tree
    while (iterator.hasNext) {
      val elem = iterator.next()
      if (isDefun(elem)) {
        val funName = elem.asInstanceOf[DefDef].name
        if (funName equals targetCalleeName) {
          callerTree = elem
        }
      }
    }

    assert(
      !(callerTree equals LitNull().asInstanceOf[CustomTree])
    ) // callerTree being equal to LitNull
    // means the above search failed.

    // now, we spawn another BFS to search within the callerTree for a TermApply.
    val treeGraph2 = TreeGraph.graphFromCustomTree(callerTree)
    val iterator2 = new BFS(treeGraph2)

    val acc = ListBuffer[(TypeName, TermName)]()

    while (iterator2.hasNext) {
      val elem = iterator2.next()
      if (elem.isInstanceOf[TermApply]) {
        val funName = elem.asInstanceOf[TermApply].fun.asInstanceOf[TermName]
        val myClasses = findMyClasses(elem, treeGraph)
        myClasses.foreach(klass => acc.+=((klass, funName)))
      }
    }
    acc.toList.distinct
  }

  /** @param customTree
    * @return
    */
  def varMutatingStatCollectorInDefun(defun: CustomTree): List[Stat] = {
    if (!defun.isInstanceOf[DefDef]) {
      throw new InvalidInput(truncatedString(defun))
    }

    def isObjectName(target: TermName): Boolean = target.value(0).isUpper

    val treeGraph = TreeGraph.graphFromCustomTree(defun)
    val iterator = new BFS(treeGraph)

    val acc = ListBuffer[Stat]()

    while (iterator.hasNext) {
      val elem = iterator.next()
      if (elem.isInstanceOf[TermAssign]) {
        acc += elem.asInstanceOf[Stat]
      }
    }
    acc.toList
  }

  /** Retrieves the parameter list of the given function definition.
    *
    * @param Function definition as the CustomTree type
    * @return Parameter list as the TermParam type
    */
  def paramListRetreiver(defun: CustomTree): TermParam = {
    if (!defun.isInstanceOf[DefDef]) {
      throw new InvalidInput(truncatedString(defun))
    }
    val treeGraph = TreeGraph.graphFromCustomTree(defun)
    val iterator = new BFS(treeGraph)

    while (iterator.hasNext) {
      val elem = iterator.next()
      if (elem.isInstanceOf[TermParam])
        return elem.asInstanceOf[TermParam]
    }

    // if control ever hits this line...
    throw new InvalidInput(truncatedString(defun))
  }

  /** @param defclass
    * @return
    */
  def stateTupMaker(defclass: CustomTree): List[(CustomTree, TermName)] = {
    if (!defclass.isInstanceOf[DefClass]) {
      throw new InvalidInput(truncatedString(defclass))
    }

    val vars = varCollector(defclass)
    // a toplevelvar is a var with ph as method name, and some initial value
    val toplevelVars = vars.filter(tup => {
      val methodName = tup._2
      val initialVal = tup._5
      methodName.equals(TermName("ph")) && !initialVal.isEmpty
    })

    // since we cannot cast this into a tuple directly, we just return a list
    val initValues = catMaybes[CustomTerm](toplevelVars.map(_._5))
    val names = toplevelVars.map(_._3)

    assert(initValues.length == names.length)

    initValues.zip(names)
  }

  /** Collects all state-mutating statements in a given defun.
    *
    * @param defun
    * @param defClass
    */
  def stateMutatingStatCollector(
      defun: CustomTree,
      defclass: CustomTree
  ): List[Stat] = {
    // Precondition check
    if (!defun.isInstanceOf[DefDef]) {
      throw new InvalidInput(truncatedString(defun))
    }
    if (!defclass.isInstanceOf[DefClass]) {
      throw new InvalidInput(truncatedString(defclass))
    }

    val vars = varCollector(defclass)

    // TODO: we need callgraph object to do this job
    ??? // TODO
  }
}
