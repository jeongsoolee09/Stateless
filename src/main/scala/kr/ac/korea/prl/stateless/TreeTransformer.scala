package kr.ac.korea.prl.stateless.TreeTransformer

import scala.annotation.tailrec
import scala.meta._

import kr.ac.korea.prl.stateless.TreeTraverser._

object TODO extends Exception

class TreeTransformer {

  /**
    * Remove all top-level vars in the class definitions.
    *
    * @param tree
    * @return
    */
  def isToplevel(varTup: (Type.Name, Term.Name, Term.Name,
                          Option[Type], Option[Term]),
                 tree: Tree) = {
    val innerClassInfo = TreeTraverser.innerClassChainCollector(tree)
    val toplevelClasses = innerClassInfo.foldLeft(List[Type.Name]())((acc, tup) =>
      if (tup._1 == Type.Name("ph")) tup._1::acc else acc
    )
    toplevelClasses.foldLeft(false)((acc, classType) =>
      (classType == varTup._1) || acc)
  }


  /**
    * Spot all toplevel vars in a tree.
    *
    * @param tree
    * @return tuples describing all toplevel vars
    */
  def toplevelVarSpotter(tree: Tree): List[(Type.Name, Term.Name, Term.Name,
                                             Option[Type], Option[Term])] =
    TreeTraverser.varCollector(tree).filter((isToplevel(_, tree)))


  /**
    * Remove all toplevel vars in a tree.
    *
    * @param tree
    * @return the same tree with toplevel vars removed.
    */
  def toplevelVarRemover(tree: Tree): Tree = {
    val toplevelVars = toplevelVarSpotter(tree)
    tree match {
      case Defn.Class(mods, name, tparams, ctor, Template(early, inits, self, stats)) => {
                        val filtered = stats.filter(toplevelVars.contains(_))
                        Defn.Class(mods, name, tparams, ctor, Template(early, inits, self, filtered))
      }
      case Source(stats) =>
        Source(stats.filter(TreeTraverser.scopeGreaterThanClass)
                 .map(toplevelVarRemover(_).asInstanceOf[Stat]))

      case Pkg(ref, stats) =>
        Pkg(ref, stats.filter(TreeTraverser.scopeGreaterThanClass)
              .map(toplevelVarRemover(_).asInstanceOf[Stat]))
    }
  }


}

