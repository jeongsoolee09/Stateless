package kr.ac.korea.prl.stateless.CustomTree

/**
  * A custom lightweigh tree that provides a layer between scala.meta.CustomTree and jgrapht graphs
  * This focuses to serve simple Scala codes.
  *
  */

import scala.meta._
import org.scalameta.tests.typecheckError

sealed trait CustomTree

/* ============ Ref ============ */
sealed trait Ref extends CustomTree

/* ============ Name ============ */
case class Name(value: String) extends Ref
case class Init(tpe: Type, name: Name, argss: List[List[Term]]) extends Ref
/* ============ Member ============ */
sealed trait Member extends CustomTree

/* ============ Member ============ */
case class Self(name: Name, decltpe: Option[Type]) extends Member {
  def apply(name: Name, decltpe: Option[Type]) = new Self(name, decltpe)
}

/* ============ Pat ============ */
sealed trait Pat extends CustomTree
case class PatVar(name: TermName) extends Pat {
  def apply(name: TermName) = new PatVar(name)
}

/* ============ Type ============ */
sealed trait Type extends CustomTree
case class TypeBounds(lo: Option[Type], hi: Option[Type]) extends Type {
  def apply(lo: Option[Type], hi: Option[Type]) = new TypeBounds(lo, hi)
}
case class TypeParam(mods: List[Mod], name: Name, tparams: List[TypeParam],
                     tbounds: TypeBounds, vbounds: List[Type], cbounds: List[Type]) extends Type {
  def apply(mods: List[Mod], name: Name, tparams: List[TypeParam],
            tbounds: TypeBounds, vbounds: List[Type], cbounds: List[Type]) =
    new TypeParam(mods, name, tparams, tbounds, vbounds, cbounds)
}
case class TypeName(value: String) extends Type {
  def apply(value: String) = new TypeName(value)
}

/* ============ Stat ============ */
sealed trait Stat extends CustomTree

case class Case(pat: Pat, cond: Option[Term], body: Term) extends CustomTree {
  def apply(pat: Pat, cond: Option[Term], body: Term) = new Case(pat, cond, body)
}

/* ============ Enumerator ============ */
sealed trait Enumerator extends CustomTree
case class Generator(pat: Pat, rhs: Term) extends Enumerator {
  def apply(pat: Pat, rhs: Term) = new Generator(pat, rhs)
}
case class CaseGenerator(pat: Pat, rhs: Term) extends Enumerator {
  def apply(pat: Pat, rhs: Term) = new CaseGenerator(pat, rhs)
}
case class Val(pat: Pat, rhs: Term) extends Enumerator {
  def apply(pat: Pat, rhs: Term) = new Val(pat, rhs)
}
case class Guard(cond: Term) extends Enumerator {
  def apply(cond: Term) = new Guard(cond)
}

/* ============ Term ============ */
sealed trait Term extends Stat
case class TermName(value: String) extends Term {
  def apply(value: String) = new TermName(value)
}
case class TermThis(qual: Name) extends Term {
  def apply(qual: Name) = new TermThis(qual)
}
case class TermSuper(thisp: Name, superp: Name) extends Term {
  def apply(thisp: Name, superp: Name) = new TermSuper(thisp, superp)
}
sealed trait TermRef extends Term with Ref
case class TermParam(mods: List[Mod], name: Name, decltpe: Option[Type], default: Option[Term]) extends Term {
  def apply(mods: List[Mod], name: Name, decltpe: Option[Type], default: Option[Term]) = new TermParam(mods, name, decltpe, default)
}
case class TermLambda(params: List[TermParam], body: Term) extends Term {
  def apply(params: List[TermParam], body: Term) = new TermLambda(params, body)
}
case class TermSelect(qual: Term, name: TermName) extends Term {
  def apply(qual: Term, name: TermName) = new TermSelect(qual, name)
}
case class TermInterpolate(prefix: TermName, parts: List[Lit], args: List[Term]) extends Term {
  def apply(prefix: TermName, parts: List[Lit], args: List[Term]) = new TermInterpolate(prefix, parts, args)
}
case class TermApply(fun: Term, args: List[Term]) extends Term {
  def apply(fun: Term, args: List[Term]) = new TermApply(fun, args)
}
case class TermApplyUsing(fun: Term, args: List[Term]) extends Term {
  def apply(fun: Term, args: List[Term]) = new TermApplyUsing(fun, args)
}
case class TermApplyInfix(lhs: Term, op: TermName, targs: List[Type], args: List[Term]) extends Term {
  def apply(lhs: Term, op: TermName, targs: List[Type], args: List[Term]) = new TermApplyInfix(lhs, op, targs, args)
}
case class TermApplyUnary(op: TermName, arg: Term) extends Term {
  def apply(op: TermName, arg: Term) = new TermApplyUnary(op, arg)
}
case class TermAssign(lhs: Term, rhs: Term) extends Term {
  def apply(lhs: Term, rhs: Term) = new TermAssign(lhs, rhs)
}
case class TermReturn(expr: Term) extends Term {
  def apply(expr: Term) = new TermReturn(expr)
}
case class TermNew(init: Init) extends Term {
  def apply(init: Init) = new TermNew(init)
}
case class TermBlock(stats: List[Stat]) extends Term {
  def apply(stats: List[Stat]) = new TermBlock(stats)
}
case class TermIf(cond: Term, thenp: Term, elsep: Term) extends Term {
  def apply(cond: Term, thenp: Term, elsep: Term) = new TermIf(cond, thenp, elsep)
}
case class TermTry(expr: Term, catchp: List[Case], finallyp: Option[Term]) extends Term {
  def apply(expr: Term, catchp: List[Case], finallyp: Option[Term]) = new TermTry(expr, catchp, finallyp)
}
case class TermWhile(expr: Term, body: Term) extends Term {
  def apply(expr: Term, body: Term) = new TermWhile(expr, body)
}
case class TermFor(enums: List[Enumerator], body: Term) extends Term {
  def apply(enums: List[Enumerator], body: Term) = new TermFor(enums, body)
}
case class TermThrow(expr: Term) extends Term {
  def apply(expr: Term) = new TermThrow(expr)
}

/* ============ Lit ============ */
sealed trait Lit extends Term with Pat with Type
case class LitInt(value: scala.Int) extends Lit {
  def apply(value: scala.Int) = new LitInt(value)
}
case class LitDouble(format: scala.Double) extends Lit {
  def apply(format: scala.Double) = new LitDouble(format)
}
case class LitFloat(format: scala.Float) extends Lit {
  def apply(format: scala.Float) = new LitFloat(format)
}
case class LitByte(value: scala.Byte) extends Lit {
  def apply(value: scala.Byte) = new LitByte(value)
}
case class LitShort(value: scala.Short) extends Lit {
  def apply(value: scala.Short) = new LitShort(value)
}
case class LitChar(value: scala.Char) extends Lit {
  def apply(value: scala.Char) = new LitChar(value)
}
case class LitLong(value: scala.Long) extends Lit {
  def apply(value: scala.Long) = new LitLong(value)
}
case class LitBoolean(value: scala.Boolean) extends Lit {
  def apply(value: scala.Boolean) = new LitBoolean(value)
}
case class LitUnit() extends Lit {
  def apply() = new LitUnit()
}
case class LitString(value: scala.Predef.String) extends Lit {
  def apply(value: scala.Predef.String) = new LitString(value)
}
case class LitSymbol(value: scala.Symbol) extends Lit {
  def apply(value: scala.Symbol) = new LitSymbol(value)
}

/* ============ Mod ============ */
sealed trait Mod extends CustomTree
case class Private(within: Ref) extends Mod {
  def apply(within: Ref) = new Private(within)
}
case class Protected(within: Ref) extends Mod {
  def apply(within: Ref) = new Protected(within)
}
case class Implicit() extends Mod {
  def apply() = new Implicit()
}
case class Abstract() extends Mod {
  def apply() = new Abstract()
}
case class Override() extends Mod {
  def apply() = new Override()
}
case class Super() extends Mod {
  def apply() = new Super()
}
case class Final() extends Mod {
  def apply() = new Final()
}

/* ============ Defn ============ */
sealed trait Defn extends Stat
case class DefVal(mods: List[Mod], pats: List[Pat], decltpe: Option[Type], rhs: Term) extends Defn {
  def apply(mods: List[Mod], pats: List[Pat], decltpe: Option[Type], rhs: Term) = new DefVal(mods, pats, decltpe, rhs)
}
case class DefVar(mods: List[Mod], pats: List[Pat], decltpe: Option[Type], rhs: Option[Term]) extends Defn {
  def apply(mods: List[Mod], pats: List[Pat], decltpe: Option[Type], rhs: Option[Term]) = new DefVar(mods, pats, decltpe, rhs)
}
case class DefDef(mods: List[Mod], name: TermName, tparams: List[TypeParam], paramss:List[List[TermParam]], decltpe: Option[Type], body: Term) extends Defn {
  def apply(mods: List[Mod], name: TermName, tparams: List[TypeParam], paramss:List[List[TermParam]], decltpe: Option[Type], body: Term) = new DefDef(mods, name, tparams, paramss, decltpe, body)
}
case class DefEnum(mods: List[Mod], name: TypeName, tparams: List[TypeParam], ctor: Primary, templ: Template) extends Defn {
  def apply(mods: List[Mod], name: TypeName, tparams: List[TypeParam], ctor: Primary, templ: Template) = new DefEnum(mods, name, tparams, ctor, templ)
}

/* ============ Ctor ============ */
sealed trait Ctor extends CustomTree with Member
case class Primary(mods: List[Mod], name: Name, paramss: List[List[TermParam]]) extends Ctor {
  def apply(mods: List[Mod], name: Name, paramss: List[List[TermParam]]) = new Primary(mods, name, paramss)
}
case class Secondary(mods: List[Mod], name: Name, paramss: List[List[TermParam]], init: Init, stats: List[Stat]) extends Ctor {
  def apply(mods: List[Mod], name: Name, paramss: List[List[TermParam]], init: Init, stats: List[Stat]) = new Secondary(mods, name, paramss, init, stats)
}

/* ============ Template ============ */
case class Template(early: List[Stat], inits: List[Init], self: Self, stats: List[Stat]) extends CustomTree {
  def apply(early: List[Stat], inits: List[Init], self: Self, stats: List[Stat]) = new Template(early, inits, self, stats)
}

/* ============ Source ============ */
case class Source(stats: List[Stat]) extends CustomTree {
  def apply(stats: List[Stat]) = new Source(stats)
}

/* ============ Pkg ============ */
case class Pkg(ref: TermRef, stats: List[Stat]) extends Stat {
  def apply(ref: TermRef, stats: List[Stat]) = new Pkg(ref, stats)
}

/* ============ Import ============ */
sealed trait Importee extends CustomTree
case class Importer(ref: TermRef, importees: List[Importee]) extends CustomTree {
  def apply(ref: TermRef, importees: List[Importee]) = new Importer(ref, importees)
}
case class ImporteeWildcard() extends Importee {
  def apply() = new ImporteeWildcard()
}
case class ImporteeGiven(tpe: Type) extends Importee {
  def apply(tpe: Type) = new ImporteeGiven(tpe)
}
case class ImporteeGivenAll() extends Importee {
  def apply() = new ImporteeGivenAll()
}
case class ImporteeName(name: Name) extends Importee {
  def apply(name: Name) = new ImporteeName(name)
}

case class Import(importers: List[Importer]) extends Stat {
  def apply(importers: List[Importer]) = new Import(importers)
}


object TODO extends Exception

class NotSupportedMetaTree(tree: Tree) extends Exception
class ThisMatchIsExhaustive() extends Exception

object CustomTreeTranslator {

  /* I left the package qualifiers just for documentation */

  def scalaMetaToCustomTree(smtree: scala.meta.Tree): CustomTree = smtree match {

    case scala.meta.Name(value) => Name(value)
    case scala.meta.Init(tpe, name, argss) =>
      Init(scalaMetaToCustomTree(tpe).asInstanceOf[Type],
           scalaMetaToCustomTree(name).asInstanceOf[Name],
           argss.map(_.map(scalaMetaToCustomTree(_).asInstanceOf[Term])))

    case scala.meta.Self(name, decltpe) =>
      Self(scalaMetaToCustomTree(name).asInstanceOf[Name],
           decltpe.map(scalaMetaToCustomTree(_).asInstanceOf[Type]))

    case current: scala.meta.Type => current match {
      case Type.Bounds(lo, hi) =>
        TypeBounds(lo.map(scalaMetaToCustomTree(_).asInstanceOf[Type]),
                   hi.map(scalaMetaToCustomTree(_).asInstanceOf[Type]))

      case Type.Param(mods, name, tparams,
                      tbounds, vbounds, cbounds) =>
        TypeParam(mods.map(scalaMetaToCustomTree(_).asInstanceOf[Mod]),
                  scalaMetaToCustomTree(name).asInstanceOf[Name],
                  tparams.map(scalaMetaToCustomTree(_).asInstanceOf[TypeParam]),
                  scalaMetaToCustomTree(tbounds).asInstanceOf[TypeBounds],
                  vbounds.map(scalaMetaToCustomTree(_).asInstanceOf[Type]),
                  cbounds.map(scalaMetaToCustomTree(_).asInstanceOf[Type]))

      case Type.Name(value) => TypeName(value)
      case otherwise => throw new NotSupportedMetaTree(otherwise)
    }

    case current: scala.meta.Enumerator => current match {
      case Enumerator.Generator(pat, rhs) =>
        Generator(scalaMetaToCustomTree(pat).asInstanceOf[Pat],
                  scalaMetaToCustomTree(rhs).asInstanceOf[Term])

      case Enumerator.CaseGenerator(pat, rhs) =>
        CaseGenerator(scalaMetaToCustomTree(pat).asInstanceOf[Pat],
                      scalaMetaToCustomTree(rhs).asInstanceOf[Term])

      case Enumerator.Val(pat, rhs) =>
        Val(scalaMetaToCustomTree(pat).asInstanceOf[Pat],
            scalaMetaToCustomTree(rhs).asInstanceOf[Term])

      case Enumerator.Guard(cond) =>
        Guard(scalaMetaToCustomTree(cond).asInstanceOf[Term])

      case otherwise => throw new NotSupportedMetaTree(otherwise)
    }

    case current: scala.meta.Term => current match {
      case Term.Name(name: Predef.String) =>
        TermName(name)

      case Term.Param(mods, name, declpe, default) =>
        TermParam(mods.map(scalaMetaToCustomTree(_).asInstanceOf[Mod]),
                  scalaMetaToCustomTree(name).asInstanceOf[Name],
                  declpe.map(scalaMetaToCustomTree(_).asInstanceOf[Type]),
                  default.map(scalaMetaToCustomTree(_).asInstanceOf[Term]))

      case Term.Super(thisp, superp) =>
        TermSuper(scalaMetaToCustomTree(thisp).asInstanceOf[Name],
                  scalaMetaToCustomTree(superp).asInstanceOf[Name])

      case Term.Param(mods, name, decltpe, default) =>
        TermParam(mods.map(scalaMetaToCustomTree(_).asInstanceOf[Mod]),
                  scalaMetaToCustomTree(name).asInstanceOf[Name],
                  decltpe.map(scalaMetaToCustomTree(_).asInstanceOf[Type]),
                  default.map(scalaMetaToCustomTree(_).asInstanceOf[Term]))

      case Term.Function(params, body) =>
        TermLambda(params.map(scalaMetaToCustomTree(_).asInstanceOf[TermParam]),
                   scalaMetaToCustomTree(body).asInstanceOf[Term])

      case Term.Select(qual, name) =>
        TermSelect(scalaMetaToCustomTree(qual).asInstanceOf[Term],
                   scalaMetaToCustomTree(name).asInstanceOf[TermName])

      case Term.Interpolate(prefix, parts, args) =>
        TermInterpolate(scalaMetaToCustomTree(prefix).asInstanceOf[TermName],
                        parts.map(scalaMetaToCustomTree(_).asInstanceOf[Lit]),
                        args.map(scalaMetaToCustomTree(_).asInstanceOf[Term]))

      case Term.Apply(fun, args) =>
        TermApply(scalaMetaToCustomTree(fun).asInstanceOf[Term],
                  args.map(scalaMetaToCustomTree(_).asInstanceOf[Term]))

      case Term.ApplyUsing(fun, args) =>
        TermApplyUsing(scalaMetaToCustomTree(fun).asInstanceOf[Term],
                       args.map(scalaMetaToCustomTree(_).asInstanceOf[Term]))

      case Term.ApplyInfix(lhs, op, targs, args) =>
        TermApplyInfix(scalaMetaToCustomTree(lhs).asInstanceOf[Term],
                       scalaMetaToCustomTree(op).asInstanceOf[TermName],
                       targs.map(scalaMetaToCustomTree(_).asInstanceOf[Type]),
                       args.map(scalaMetaToCustomTree(_).asInstanceOf[Term]))

      case Term.ApplyUnary(op, arg) =>
        TermApplyUnary(scalaMetaToCustomTree(op).asInstanceOf[TermName],
                       scalaMetaToCustomTree(arg).asInstanceOf[Term])

      case Term.Assign(lhs, rhs) =>
        TermAssign(scalaMetaToCustomTree(lhs).asInstanceOf[Term],
                   scalaMetaToCustomTree(rhs).asInstanceOf[Term])

      case Term.Return(expr) =>
        TermReturn(scalaMetaToCustomTree(expr).asInstanceOf[Term])

      case Term.New(init) =>
        TermNew(scalaMetaToCustomTree(init).asInstanceOf[Init])

      case Term.Block(stats) =>
        TermBlock(stats.map(scalaMetaToCustomTree(_).asInstanceOf[Stat]))

      case Term.If(cond, thenp, elsep) =>
        TermIf(scalaMetaToCustomTree(cond).asInstanceOf[Term],
               scalaMetaToCustomTree(thenp).asInstanceOf[Term],
               scalaMetaToCustomTree(elsep).asInstanceOf[Term])

      case Term.Try(expr, catchp, finallyp) =>
        TermTry(scalaMetaToCustomTree(expr).asInstanceOf[Term],
                catchp.map(scalaMetaToCustomTree(_).asInstanceOf[Case]),
                finallyp.map(scalaMetaToCustomTree(_).asInstanceOf[Term]))

      case Term.While(expr, body) =>
        TermWhile(scalaMetaToCustomTree(expr).asInstanceOf[Term],
                  scalaMetaToCustomTree(body).asInstanceOf[Term])

      case Term.For(enums, body) =>
        TermFor(enums.map(scalaMetaToCustomTree(_).asInstanceOf[Enumerator]),
                scalaMetaToCustomTree(body).asInstanceOf[Term])

      case Term.Throw(expr) =>
        TermThrow(scalaMetaToCustomTree(expr).asInstanceOf[Term])

      case current: scala.meta.Lit => current match {
        case Lit.Int(value) => LitInt(value)
        case Lit.Double(format) => LitDouble(format.toDouble)
        case Lit.Float(format) => LitFloat(format.toFloat)
        case Lit.Byte(value) => LitByte(value)
        case Lit.Short(value) => LitShort(value)
        case Lit.Char(value) => LitChar(value)
        case Lit.Long(value) => LitLong(value)
        case Lit.Boolean(value) => LitBoolean(value)
        case Lit.Unit() => LitUnit()
        case Lit.String(value) => LitString(value)
        case Lit.Symbol(value) => LitSymbol(value)
        case otherwise => throw new NotSupportedMetaTree(otherwise)
      }

      case otherwise => throw new NotSupportedMetaTree(otherwise)
    }

    case current: scala.meta.Mod => current match {
      case Mod.Private(within) => Private(scalaMetaToCustomTree(within).asInstanceOf[Ref])
      case Mod.Protected(within) => Protected(scalaMetaToCustomTree(within).asInstanceOf[Ref])
      case Mod.Implicit() => Implicit()
      case Mod.Abstract() => Abstract()
      case Mod.Override() => Override()
      case Mod.Super() => Super()
      case Mod.Final() => Final()
      case otherwise => throw new NotSupportedMetaTree(otherwise)
    }

    case current: scala.meta.Defn => current match {
      case Defn.Val(mods, pats, decltpe, rhs) =>
        DefVal(mods.map(scalaMetaToCustomTree(_).asInstanceOf[Mod]),
               pats.map(scalaMetaToCustomTree(_).asInstanceOf[Pat]),
               decltpe.map(scalaMetaToCustomTree(_).asInstanceOf[Type]),
               scalaMetaToCustomTree(rhs).asInstanceOf[Term])

      case Defn.Var(mods, pats, decltpe, rhs) =>
        DefVar(mods.map(scalaMetaToCustomTree(_).asInstanceOf[Mod]),
               pats.map(scalaMetaToCustomTree(_).asInstanceOf[Pat]),
               decltpe.map(scalaMetaToCustomTree(_).asInstanceOf[Type]),
               rhs.map(scalaMetaToCustomTree(_).asInstanceOf[Term]))

      case Defn.Def(mods, name, tparams, paramss, decltpe, body) =>
        DefDef(mods.map(scalaMetaToCustomTree(_).asInstanceOf[Mod]),
               scalaMetaToCustomTree(name).asInstanceOf[TermName],
               tparams.map(scalaMetaToCustomTree(_).asInstanceOf[TypeParam]),
               paramss.map(_.map(scalaMetaToCustomTree(_).asInstanceOf[TermParam])),
               decltpe.map(scalaMetaToCustomTree(_).asInstanceOf[Type]),
               scalaMetaToCustomTree(body).asInstanceOf[Term])

      case Defn.Enum(mods, name, tparams, ctor, templ) =>
        DefEnum(mods.map(scalaMetaToCustomTree(_).asInstanceOf[Mod]),
                scalaMetaToCustomTree(name).asInstanceOf[TypeName],
                tparams.map(scalaMetaToCustomTree(_).asInstanceOf[TypeParam]),
                scalaMetaToCustomTree(ctor).asInstanceOf[Primary],
                scalaMetaToCustomTree(templ).asInstanceOf[Template])

      case otherwise => throw new NotSupportedMetaTree(otherwise)
    }

    case scala.meta.Template(early, inits, self, stats) =>
      Template(early.map(scalaMetaToCustomTree(_).asInstanceOf[Stat]),
               inits.map(scalaMetaToCustomTree(_).asInstanceOf[Init]),
               scalaMetaToCustomTree(self).asInstanceOf[Self],
               stats.map(scalaMetaToCustomTree(_).asInstanceOf[Stat]))

    case scala.meta.Source(stats) =>
      Source(stats.map(scalaMetaToCustomTree(_).asInstanceOf[Stat]))

    case scala.meta.Pkg(ref, stats) =>
      Pkg(scalaMetaToCustomTree(ref).asInstanceOf[TermRef],
          stats.map(scalaMetaToCustomTree(_).asInstanceOf[Stat]))

    case scala.meta.Importer(ref, importees) =>
      Importer(scalaMetaToCustomTree(ref).asInstanceOf[TermRef],
               importees.map(scalaMetaToCustomTree(_).asInstanceOf[Importee]))

    case current: scala.meta.Importee => current match {
      case Importee.Wildcard() => ImporteeWildcard()
      case Importee.Given(tpe: Type) => ImporteeGiven(scalaMetaToCustomTree(tpe).asInstanceOf[Type])
      case Importee.GivenAll() => ImporteeGivenAll()
      case Importee.Name(name: Name) => ImporteeName(scalaMetaToCustomTree(name).asInstanceOf[Name])
      case otherwise => throw new NotSupportedMetaTree(otherwise)
    }

    case current: Pat => current match {
      case Pat.Var(name: Term.Name) => PatVar(scalaMetaToCustomTree(name).asInstanceOf[TermName])
      case otherwise => throw new NotSupportedMetaTree(otherwise)
    }

    case otherwise => throw new NotSupportedMetaTree(otherwise)
  }

  // do a pattern matching on (ctree: CustomTree)
  def customTreeToScalaMeta(ctree: CustomTree): scala.meta.Tree = ctree match {

    /* ============ Import ============ */
    case Importer(ref: TermRef, importees: List[Importee]) => throw TODO


    case Name(value: String) => throw TODO
    case Init(tpe: Term, name: Name, argss: List[List[Term]]) => throw TODO

    case Self(name: Name, decltpe: Option[Type]) => throw TODO

    case current: Type => current match {
      case TypeBounds(lo: Option[Type], hi: Option[Type]) => throw TODO
      case TypeParam(mods: List[Mod],
                     name: Name,
                     tparams: List[TypeParam],
                     tbounds: TypeBounds,
                     vbounds: List[Type],
                     cbounds: List[Type]) => throw TODO
      case TypeName(value: String) => throw TODO
      case _ => throw new ThisMatchIsExhaustive()
    }

    case current: Enumerator => current match {
      case Generator(pat: Pat, rhs: Term) => throw TODO
      case CaseGenerator(pat: Pat, rhs: Term) => throw TODO
      case Val(pat: Pat, rhs: Term) => throw TODO
      case Guard(cond: Term) => throw TODO
    }


    /* ============ Mod ============ */
    case current: Mod => current match {
      case Private(within: Ref) => throw TODO
      case Protected(within: Ref) => throw TODO
      case Implicit() => throw TODO
      case Abstract() => throw TODO
      case Override() => throw TODO
      case Super() => throw TODO
      case Final() => throw TODO
    }

    case current: Stat => current match {

      /* ============ Pkg ============ */
      case Pkg(ref: TermRef, stats: List[Stat]) => throw TODO

      /* ============ Term ============ */
      case current: Term => current match {
        case TermName(value: String) => throw TODO
        case TermThis(qual: Name) => throw TODO
        case TermSuper(thisp: Name, superp: Name) => throw TODO
        case TermParam(mods: List[Mod], name: Name,
                       decltpe: Option[Type], default: Option[Term]) => throw TODO
        case TermLambda(params: List[TermParam], body: Term) => throw TODO
        case TermSelect(qual: Term, name: TermName) => throw TODO
        case TermInterpolate(prefix: Name, parts: List[Lit],
                             args: List[Term]) => throw TODO
        case TermApply(fun: Term, args: List[Term]) => throw TODO
        case TermApplyUsing(fun: Term, args: List[Term]) => throw TODO
        case TermApplyInfix(lhs: Term, op: Name,
                            targs: List[Type], args: List[Term]) => throw TODO
        case TermApplyUnary(op: Name, arg: Term) => throw TODO
        case TermAssign(lhs: Term, rhs: Term) => throw TODO
        case TermReturn(expr: Term) => throw TODO
        case TermNew(init: Init) => throw TODO
        case TermBlock(stats: List[Stat]) => throw TODO
        case TermIf(cond: Term, thenp: Term, elsep: Term) => throw TODO
        case TermTry(expr: Term, catchp: List[Case], finallyp: Option[Term]) => throw TODO
        case TermWhile(expr: Term, body: Term) => throw TODO
        case TermFor(enums: List[Enumerator], body: Term) => throw TODO
        case TermThrow(expr: Term) => throw TODO
        case current: TermRef => throw TODO

        /* ============ Lit ============ */
        case current: Lit => current match {
          case LitInt(value) => throw TODO
          case LitDouble(format) => throw TODO
          case LitFloat(format) => throw TODO
          case LitByte(value) => throw TODO
          case LitShort(value) => throw TODO
          case LitChar(value) => throw TODO
          case LitLong(value) => throw TODO
          case LitBoolean(value) => throw TODO
          case LitUnit() => throw TODO
          case LitString(value) => throw TODO
          case LitSymbol(value) => throw TODO
          case _ => throw new ThisMatchIsExhaustive()
        }

        case _ => throw new ThisMatchIsExhaustive()
      }

      /* ============ Defn ============ */
      case current: Defn => current match {
        case DefVal(mods: List[Mod], pats: List[Pat], decltpe: Type, rhs) => throw TODO
        case DefVar(mods: List[Mod], pats: List[Pat], decltpe: Type, rhs) => throw TODO
        case DefDef(mods: List[Mod], name: TermName,
                    tparams: List[TypeParam], params:List[List[TermParam]],
                    decltpe: Type, body: Term) => throw TODO
        case DefEnum(mods: List[Mod], name: TypeName,
                     tparams: List[TypeParam], ctor: Primary,
                     templ: Template) => throw TODO
        case _ => throw new ThisMatchIsExhaustive()
      }

      case Import(importers: List[Importer]) => throw TODO
    }


    /* ============ Ctor ============ */
    case current: Ctor => current match {
      case Primary(mods: List[Mod], name: Name,
                   paramss: List[List[TermParam]]) => throw TODO
      case Secondary(mods: List[Mod], name: Name,
                     paramss: List[List[TermParam]], init: Init,
                     stats: List[Stat]) => throw TODO

    }

    /* ============ Template ============ */
    case Template(early: List[Stat], inits: List[Init],
                  self: Self, stats: List[Stat]) => throw TODO

    /* ============ Source ============ */
    case Source(stats: List[Stat]) => throw TODO

    case current: Importee => current match {
      case ImporteeWildcard() => throw TODO
      case ImporteeGiven(tpe: Type) => throw TODO
      case ImporteeGivenAll() => throw TODO
      case ImporteeName(name: Name) => throw TODO
    }

    /* ============ Pat ============ */
    case current: Pat => current match {
      case PatVar(name: TermName) => throw TODO
      case _ => throw new ThisMatchIsExhaustive()
    }


    case _ => throw new ThisMatchIsExhaustive()
  }

}
