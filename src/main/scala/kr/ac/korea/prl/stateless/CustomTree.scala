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


/* ============ CustomName ============ */
case class CustomName(value: String) extends Ref
case class CustomInit(tpe: CustomType,
                      name: CustomName,
                      argss: List[List[CustomTerm]]) extends Ref


/* ============ Member ============ */
sealed trait Member extends CustomTree


/* ============ Member ============ */
case class Self(name: CustomName,
                decltpe: Option[CustomType]) extends Member {
  def apply(name: CustomName,
            decltpe: Option[CustomType]) = new Self(name,
                                                    decltpe)
}


/* ============ CustomPat ============ */
sealed trait CustomPat extends CustomTree
case class PatVar(name: TermName) extends CustomPat {
  def apply(name: TermName) = new PatVar(name)
}


/* ============ CustomType ============ */
sealed trait CustomType extends CustomTree
case class TypeBounds(lo: Option[CustomType],
                      hi: Option[CustomType]) extends CustomType {
  def apply(lo: Option[CustomType],
            hi: Option[CustomType]) = new TypeBounds(lo,
                                                     hi)
}

case class TypeParam(mods: List[CustomMod],
                     name: CustomName,
                     tparams: List[TypeParam],

  tbounds: TypeBounds,
                     vbounds: List[CustomType],
                     cbounds: List[CustomType]) extends CustomType {
  def apply(mods: List[CustomMod],
            name: CustomName,
            tparams: List[TypeParam],

    tbounds: TypeBounds,
            vbounds: List[CustomType],
            cbounds: List[CustomType]) =
    new TypeParam(mods,
                  name,
                  tparams,
                  tbounds,
                  vbounds,
                  cbounds)
}

case class TypeName(value: String) extends CustomType {
  def apply(value: String) = new TypeName(value)
}


/* ============ CustomStat ============ */
sealed trait CustomStat extends CustomTree

case class Case(pat: CustomPat,
                cond: Option[CustomTerm],
                body: CustomTerm) extends CustomTree {
  def apply(pat: CustomPat,
            cond: Option[CustomTerm],
            body: CustomTerm) = new Case(pat,
                                         cond,
                                         body)
}


/* ============ Enumerator ============ */
sealed trait Enumerator extends CustomTree
case class Generator(pat: CustomPat,
                     rhs: CustomTerm) extends Enumerator {
  def apply(pat: CustomPat,
            rhs: CustomTerm) = new Generator(pat,
                                             rhs)
}

case class CaseGenerator(pat: CustomPat,
                         rhs: CustomTerm) extends Enumerator {
  def apply(pat: CustomPat,
            rhs: CustomTerm) = new CaseGenerator(pat,
                                                 rhs)
}

case class Val(pat: CustomPat,
               rhs: CustomTerm) extends Enumerator {
  def apply(pat: CustomPat,
            rhs: CustomTerm) = new Val(pat,
                                       rhs)
}

case class Guard(cond: CustomTerm) extends Enumerator {
  def apply(cond: CustomTerm) = new Guard(cond)
}


/* ============ CustomTerm ============ */
sealed trait CustomTerm extends CustomStat
case class TermName(value: String) extends CustomTerm {
  def apply(value: String) = new TermName(value)
}

case class TermThis(qual: CustomName) extends CustomTerm {
  def apply(qual: CustomName) = new TermThis(qual)
}

case class TermSuper(thisp: CustomName,
                     superp: CustomName) extends CustomTerm {
  def apply(thisp: CustomName,
            superp: CustomName) = new TermSuper(thisp,
                                                superp)
}

sealed trait TermRef extends CustomTerm with Ref
case class TermParam(mods: List[CustomMod],
                     name: CustomName,
                     decltpe: Option[CustomType],
                     default: Option[CustomTerm]) extends CustomTerm {
  def apply(mods: List[CustomMod],
            name: CustomName,
            decltpe: Option[CustomType],
            default: Option[CustomTerm]) = new TermParam(mods,
                                                         name,
                                                         decltpe,
                                                         default)
}

case class TermLambda(params: List[TermParam],
                      body: CustomTerm) extends CustomTerm {
  def apply(params: List[TermParam],
            body: CustomTerm) = new TermLambda(params,
                                               body)
}

case class TermSelect(qual: CustomTerm,
                      name: TermName) extends CustomTerm {
  def apply(qual: CustomTerm,
            name: TermName) = new TermSelect(qual,
                                             name)
}

case class TermInterpolate(prefix: TermName,
                           parts: List[CustomLit],
                           args: List[CustomTerm]) extends CustomTerm {
  def apply(prefix: TermName,
            parts: List[CustomLit],
            args: List[CustomTerm]) = new TermInterpolate(prefix,
                                                          parts,
                                                          args)
}

case class TermApply(fun: CustomTerm,
                     args: List[CustomTerm]) extends CustomTerm {
  def apply(fun: CustomTerm,
            args: List[CustomTerm]) = new TermApply(fun,
                                                    args)
}

case class TermApplyUsing(fun: CustomTerm,
                          args: List[CustomTerm]) extends CustomTerm {
  def apply(fun: CustomTerm,
            args: List[CustomTerm]) = new TermApplyUsing(fun,
                                                         args)
}

case class TermApplyInfix(lhs: CustomTerm,
                          op: TermName,
                          targs: List[CustomType],
                          args: List[CustomTerm]) extends CustomTerm {
  def apply(lhs: CustomTerm,
            op: TermName,
            targs: List[CustomType],
            args: List[CustomTerm]) = new TermApplyInfix(lhs,
                                                         op,
                                                         targs,
                                                         args)
}

case class TermApplyUnary(op: TermName,
                          arg: CustomTerm) extends CustomTerm {
  def apply(op: TermName,
            arg: CustomTerm) = new TermApplyUnary(op,
                                                  arg)
}

case class TermAssign(lhs: CustomTerm,
                      rhs: CustomTerm) extends CustomTerm {
  def apply(lhs: CustomTerm,
            rhs: CustomTerm) = new TermAssign(lhs,
                                              rhs)
}

case class TermReturn(expr: CustomTerm) extends CustomTerm {
  def apply(expr: CustomTerm) = new TermReturn(expr)
}

case class TermNew(init: CustomInit) extends CustomTerm {
  def apply(init: CustomInit) = new TermNew(init)
}

case class TermBlock(stats: List[CustomStat]) extends CustomTerm {
  def apply(stats: List[CustomStat]) = new TermBlock(stats)
}

case class TermIf(cond: CustomTerm,
                  thenp: CustomTerm,
                  elsep: CustomTerm) extends CustomTerm {
  def apply(cond: CustomTerm,
            thenp: CustomTerm,
            elsep: CustomTerm) = new TermIf(cond,
                                            thenp,
                                            elsep)
}

case class TermTry(expr: CustomTerm,
                   catchp: List[Case],
                   finallyp: Option[CustomTerm]) extends CustomTerm {
  def apply(expr: CustomTerm,
            catchp: List[Case],
            finallyp: Option[CustomTerm]) = new TermTry(expr,
                                                        catchp,
                                                        finallyp)
}

case class TermWhile(expr: CustomTerm,
                     body: CustomTerm) extends CustomTerm {
  def apply(expr: CustomTerm,
            body: CustomTerm) = new TermWhile(expr,
                                              body)
}

case class TermFor(enums: List[Enumerator],
                   body: CustomTerm) extends CustomTerm {
  def apply(enums: List[Enumerator],
            body: CustomTerm) = new TermFor(enums,
                                            body)
}

case class TermThrow(expr: CustomTerm) extends CustomTerm {
  def apply(expr: CustomTerm) = new TermThrow(expr)
}


/* ============ CustomLit ============ */
sealed trait CustomLit extends CustomTerm with CustomPat with CustomType
case class LitInt(value: scala.Int) extends CustomLit {
  def apply(value: scala.Int) = new LitInt(value)
}

case class LitDouble(format: scala.Double) extends CustomLit {
  def apply(format: scala.Double) = new LitDouble(format)
}

case class LitFloat(format: scala.Float) extends CustomLit {
  def apply(format: scala.Float) = new LitFloat(format)
}

case class LitByte(value: scala.Byte) extends CustomLit {
  def apply(value: scala.Byte) = new LitByte(value)
}

case class LitShort(value: scala.Short) extends CustomLit {
  def apply(value: scala.Short) = new LitShort(value)
}

case class LitChar(value: scala.Char) extends CustomLit {
  def apply(value: scala.Char) = new LitChar(value)
}

case class LitLong(value: scala.Long) extends CustomLit {
  def apply(value: scala.Long) = new LitLong(value)
}

case class LitBoolean(value: scala.Boolean) extends CustomLit {
  def apply(value: scala.Boolean) = new LitBoolean(value)
}

case class LitUnit() extends CustomLit {
  def apply() = new LitUnit()
}

case class LitString(value: scala.Predef.String) extends CustomLit {
  def apply(value: scala.Predef.String) = new LitString(value)
}

case class LitSymbol(value: scala.Symbol) extends CustomLit {
  def apply(value: scala.Symbol) = new LitSymbol(value)
}


/* ============ CustomMod ============ */
sealed trait CustomMod extends CustomTree
case class Private(within: Ref) extends CustomMod {
  def apply(within: Ref) = new Private(within)
}

case class Protected(within: Ref) extends CustomMod {
  def apply(within: Ref) = new Protected(within)
}

case class Implicit() extends CustomMod {
  def apply() = new Implicit()
}

case class Abstract() extends CustomMod {
  def apply() = new Abstract()
}

case class Override() extends CustomMod {
  def apply() = new Override()
}

case class Super() extends CustomMod {
  def apply() = new Super()
}

case class Final() extends CustomMod {
  def apply() = new Final()
}


/* ============ Decl ============ */

sealed trait Decl extends CustomStat

case class DeclVal(mods: List[CustomMod], pats: List[CustomPat], decltpe: CustomType) extends Decl
case class DeclVar(mods: List[CustomMod], pats: List[CustomPat], decltpe: CustomType) extends Decl
case class DeclDef(
  mods: List[CustomMod],
  name: TermName,
  tparams: List[TypeParam],
  paramss: List[List[TermParam]],
  decltpe: CustomType) extends Decl


/* ============ Defn ============ */
sealed trait Defn extends CustomStat
case class DefVal(mods: List[CustomMod],
                  pats: List[CustomPat],
                  decltpe: Option[CustomType],
                  rhs: CustomTerm) extends Defn {
  def apply(mods: List[CustomMod],
            pats: List[CustomPat],
            decltpe: Option[CustomType],
            rhs: CustomTerm) = new DefVal(mods,
                                          pats,
                                          decltpe,
                                          rhs)
}

case class DefVar(mods: List[CustomMod],
                  pats: List[CustomPat],
                  decltpe: Option[CustomType],
                  rhs: Option[CustomTerm]) extends Defn {
  def apply(mods: List[CustomMod],
            pats: List[CustomPat],
            decltpe: Option[CustomType],
            rhs: Option[CustomTerm]) = new DefVar(mods,
                                                  pats,
                                                  decltpe,
                                                  rhs)
}

case class DefDef(mods: List[CustomMod],
                  name: TermName,
                  tparams: List[TypeParam],
                  paramss:List[List[TermParam]],
                  decltpe: Option[CustomType],
                  body: CustomTerm) extends Defn {
  def apply(mods: List[CustomMod],
            name: TermName,
            tparams: List[TypeParam],
            paramss:List[List[TermParam]],
            decltpe: Option[CustomType],
            body: CustomTerm) = new DefDef(mods,
                                           name,
                                           tparams,
                                           paramss,
                                           decltpe,
                                           body)
}

case class DefEnum(mods: List[CustomMod],
                   name: TypeName,
                   tparams: List[TypeParam],
                   ctor: PrimaryCtor,
                   templ: CustomTemplate) extends Defn {
  def apply(mods: List[CustomMod],
            name: TypeName,
            tparams: List[TypeParam],
            ctor: PrimaryCtor,
            templ: CustomTemplate) = new DefEnum(mods,
                                                 name,
                                                 tparams,
                                                 ctor,
                                                 templ)
}

case class DefClass(mods: List[CustomMod],
                    name: TypeName,
                    tparams: List[TypeParam],
                    ctor: PrimaryCtor,
                    templ: CustomTemplate) extends Defn {
  def apply(mods: List[CustomMod],
            name: TypeName,
            tparams: List[TypeParam],
            ctor: PrimaryCtor,
            templ: CustomTemplate) = new DefEnum(mods,
                                                 name,
                                                 tparams,
                                                 ctor,
                                                 templ)
}

case class DefObject(mods: List[CustomMod],
                     name: TypeName,
                     templ: CustomTemplate) extends Defn {
  def apply(mods: List[CustomMod],
            name: TypeName,
            tparams: List[TypeParam],
            ctor: PrimaryCtor,
            templ: CustomTemplate) = new DefEnum(mods,
                                                 name,
                                                 tparams,
                                                 ctor,
                                                 templ)
}

case class DefTrait(mods: List[CustomMod],
                    name: TypeName,
                    tparams: List[TypeParam],
                    ctor: PrimaryCtor,
                    templ: CustomTemplate) extends Defn


/* ============ Ctor ============ */
sealed trait Ctor extends CustomTree with Member
case class PrimaryCtor(mods: List[CustomMod],
                       name: CustomName,
                       paramss: List[List[TermParam]]) extends Ctor {
  def apply(mods: List[CustomMod],
            name: CustomName,
            paramss: List[List[TermParam]]) = new PrimaryCtor(mods,
                                                              name,
                                                              paramss)
}

case class SecondaryCtor(mods: List[CustomMod],
                         name: CustomName,
                         paramss: List[List[TermParam]],
                         init: CustomInit,
                         stats: List[CustomStat]) extends Ctor {
  def apply(mods: List[CustomMod],
            name: CustomName,
            paramss: List[List[TermParam]],
            init: CustomInit,
            stats: List[CustomStat]) = new SecondaryCtor(mods,
                                                         name,
                                                         paramss,
                                                         init,
                                                         stats)
}


/* ============ CustomTemplate ============ */
case class CustomTemplate(early: List[CustomStat],
                          inits: List[CustomInit],
                          self: Self,
                          stats: List[CustomStat]) extends CustomTree {
  def apply(early: List[CustomStat],
            inits: List[CustomInit],
            self: Self,
            stats: List[CustomStat]) = new CustomTemplate(early,
                                                          inits,
                                                          self,
                                                          stats)
}

/* ============ CustomSource ============ */
case class CustomSource(stats: List[CustomStat]) extends CustomTree {
  def apply(stats: List[CustomStat]) = new CustomSource(stats)
}


/* ============ CustomPkg ============ */
case class CustomPkg(ref: TermRef,
                     stats: List[CustomStat]) extends CustomStat {
  def apply(ref: TermRef,
            stats: List[CustomStat]) = new CustomPkg(ref,
                                                     stats)
}


/* ============ CustomImport ============ */
sealed trait Importee extends CustomTree
case class Importer(ref: TermRef,
                    importees: List[Importee]) extends CustomTree {
  def apply(ref: TermRef,
            importees: List[Importee]) = new Importer(ref,
                                                      importees)
}

case class ImporteeWildcard() extends Importee {
  def apply() = new ImporteeWildcard()
}

case class ImporteeGiven(tpe: CustomType) extends Importee {
  def apply(tpe: CustomType) = new ImporteeGiven(tpe)
}

case class ImporteeGivenAll() extends Importee {
  def apply() = new ImporteeGivenAll()
}

case class ImporteeName(name: CustomName) extends Importee {
  def apply(name: CustomName) = new ImporteeName(name)
}

case class CustomImport(importers: List[Importer]) extends CustomStat {
  def apply(importers: List[Importer]) = new CustomImport(importers)
}


class NotSupportedMetaTree(tree: Tree) extends Exception
class ThisMatchIsExhaustive() extends Exception

object CustomTreeTranslator {

  /* I left the package qualifiers just for documentation */

  def scalaMetaToCustomTree(smtree: scala.meta.Tree): CustomTree = smtree match {

    case scala.meta.Name(value) => CustomName(value)
    case scala.meta.Init(tpe, name, argss) =>
      CustomInit(scalaMetaToCustomTree(tpe).asInstanceOf[CustomType],
                 scalaMetaToCustomTree(name).asInstanceOf[CustomName],
                 argss.map(_.map(scalaMetaToCustomTree(_).asInstanceOf[CustomTerm])))

    case scala.meta.Self(name, decltpe) =>
      Self(scalaMetaToCustomTree(name).asInstanceOf[CustomName],
           decltpe.map(scalaMetaToCustomTree(_).asInstanceOf[CustomType]))

    case current: scala.meta.Type => current match {
      case Type.Bounds(lo, hi) =>
        TypeBounds(lo.map(scalaMetaToCustomTree(_).asInstanceOf[CustomType]),
                   hi.map(scalaMetaToCustomTree(_).asInstanceOf[CustomType]))

      case Type.Param(mods, name, tparams,
                      tbounds, vbounds, cbounds) =>
        TypeParam(mods.map(scalaMetaToCustomTree(_).asInstanceOf[CustomMod]),
                  scalaMetaToCustomTree(name).asInstanceOf[CustomName],
                  tparams.map(scalaMetaToCustomTree(_).asInstanceOf[TypeParam]),
                  scalaMetaToCustomTree(tbounds).asInstanceOf[TypeBounds],
                  vbounds.map(scalaMetaToCustomTree(_).asInstanceOf[CustomType]),
                  cbounds.map(scalaMetaToCustomTree(_).asInstanceOf[CustomType]))

      case Type.Name(value) => TypeName(value)
      case otherwise => throw new NotSupportedMetaTree(otherwise)
    }

    case current: scala.meta.Enumerator => current match {
      case Enumerator.Generator(pat, rhs) =>
        Generator(scalaMetaToCustomTree(pat).asInstanceOf[CustomPat],
                  scalaMetaToCustomTree(rhs).asInstanceOf[CustomTerm])

      case Enumerator.CaseGenerator(pat, rhs) =>
        CaseGenerator(scalaMetaToCustomTree(pat).asInstanceOf[CustomPat],
                      scalaMetaToCustomTree(rhs).asInstanceOf[CustomTerm])

      case Enumerator.Val(pat, rhs) =>
        Val(scalaMetaToCustomTree(pat).asInstanceOf[CustomPat],
            scalaMetaToCustomTree(rhs).asInstanceOf[CustomTerm])

      case Enumerator.Guard(cond) =>
        Guard(scalaMetaToCustomTree(cond).asInstanceOf[CustomTerm])

      case otherwise => throw new NotSupportedMetaTree(otherwise)
    }

    case current: scala.meta.Term => current match {
      case Term.Name(name: Predef.String) =>
        TermName(name)

      case Term.Param(mods, name, declpe, default) =>
        TermParam(mods.map(scalaMetaToCustomTree(_).asInstanceOf[CustomMod]),
                  scalaMetaToCustomTree(name).asInstanceOf[CustomName],
                  declpe.map(scalaMetaToCustomTree(_).asInstanceOf[CustomType]),
                  default.map(scalaMetaToCustomTree(_).asInstanceOf[CustomTerm]))

      case Term.Super(thisp, superp) =>
        TermSuper(scalaMetaToCustomTree(thisp).asInstanceOf[CustomName],
                  scalaMetaToCustomTree(superp).asInstanceOf[CustomName])

      case Term.Param(mods, name, decltpe, default) =>
        TermParam(mods.map(scalaMetaToCustomTree(_).asInstanceOf[CustomMod]),
                  scalaMetaToCustomTree(name).asInstanceOf[CustomName],
                  decltpe.map(scalaMetaToCustomTree(_).asInstanceOf[CustomType]),
                  default.map(scalaMetaToCustomTree(_).asInstanceOf[CustomTerm]))

      case Term.Function(params, body) =>
        TermLambda(params.map(scalaMetaToCustomTree(_).asInstanceOf[TermParam]),
                   scalaMetaToCustomTree(body).asInstanceOf[CustomTerm])

      case Term.Select(qual, name) =>
        TermSelect(scalaMetaToCustomTree(qual).asInstanceOf[CustomTerm],
                   scalaMetaToCustomTree(name).asInstanceOf[TermName])

      case Term.Interpolate(prefix, parts, args) =>
        TermInterpolate(scalaMetaToCustomTree(prefix).asInstanceOf[TermName],
                        parts.map(scalaMetaToCustomTree(_).asInstanceOf[CustomLit]),
                        args.map(scalaMetaToCustomTree(_).asInstanceOf[CustomTerm]))

      case Term.Apply(fun, args) =>
        TermApply(scalaMetaToCustomTree(fun).asInstanceOf[CustomTerm],
                  args.map(scalaMetaToCustomTree(_).asInstanceOf[CustomTerm]))

      case Term.ApplyUsing(fun, args) =>
        TermApplyUsing(scalaMetaToCustomTree(fun).asInstanceOf[CustomTerm],
                       args.map(scalaMetaToCustomTree(_).asInstanceOf[CustomTerm]))

      case Term.ApplyInfix(lhs, op, targs, args) =>
        TermApplyInfix(scalaMetaToCustomTree(lhs).asInstanceOf[CustomTerm],
                       scalaMetaToCustomTree(op).asInstanceOf[TermName],
                       targs.map(scalaMetaToCustomTree(_).asInstanceOf[CustomType]),
                       args.map(scalaMetaToCustomTree(_).asInstanceOf[CustomTerm]))

      case Term.ApplyUnary(op, arg) =>
        TermApplyUnary(scalaMetaToCustomTree(op).asInstanceOf[TermName],
                       scalaMetaToCustomTree(arg).asInstanceOf[CustomTerm])

      case Term.Assign(lhs, rhs) =>
        TermAssign(scalaMetaToCustomTree(lhs).asInstanceOf[CustomTerm],
                   scalaMetaToCustomTree(rhs).asInstanceOf[CustomTerm])

      case Term.Return(expr) =>
        TermReturn(scalaMetaToCustomTree(expr).asInstanceOf[CustomTerm])

      case Term.New(init) =>
        TermNew(scalaMetaToCustomTree(init).asInstanceOf[CustomInit])

      case Term.Block(stats) =>
        TermBlock(stats.map(scalaMetaToCustomTree(_).asInstanceOf[CustomStat]))

      case Term.If(cond, thenp, elsep) =>
        TermIf(scalaMetaToCustomTree(cond).asInstanceOf[CustomTerm],
               scalaMetaToCustomTree(thenp).asInstanceOf[CustomTerm],
               scalaMetaToCustomTree(elsep).asInstanceOf[CustomTerm])

      case Term.Try(expr, catchp, finallyp) =>
        TermTry(scalaMetaToCustomTree(expr).asInstanceOf[CustomTerm],
                catchp.map(scalaMetaToCustomTree(_).asInstanceOf[Case]),
                finallyp.map(scalaMetaToCustomTree(_).asInstanceOf[CustomTerm]))

      case Term.While(expr, body) =>
        TermWhile(scalaMetaToCustomTree(expr).asInstanceOf[CustomTerm],
                  scalaMetaToCustomTree(body).asInstanceOf[CustomTerm])

      case Term.For(enums, body) =>
        TermFor(enums.map(scalaMetaToCustomTree(_).asInstanceOf[Enumerator]),
                scalaMetaToCustomTree(body).asInstanceOf[CustomTerm])

      case Term.Throw(expr) =>
        TermThrow(scalaMetaToCustomTree(expr).asInstanceOf[CustomTerm])

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
        DefVal(mods.map(scalaMetaToCustomTree(_).asInstanceOf[CustomMod]),
               pats.map(scalaMetaToCustomTree(_).asInstanceOf[CustomPat]),
               decltpe.map(scalaMetaToCustomTree(_).asInstanceOf[CustomType]),
               scalaMetaToCustomTree(rhs).asInstanceOf[CustomTerm])

      case Defn.Var(mods, pats, decltpe, rhs) =>
        DefVar(mods.map(scalaMetaToCustomTree(_).asInstanceOf[CustomMod]),
               pats.map(scalaMetaToCustomTree(_).asInstanceOf[CustomPat]),
               decltpe.map(scalaMetaToCustomTree(_).asInstanceOf[CustomType]),
               rhs.map(scalaMetaToCustomTree(_).asInstanceOf[CustomTerm]))

      case Defn.Def(mods, name, tparams, paramss, decltpe, body) =>
        DefDef(mods.map(scalaMetaToCustomTree(_).asInstanceOf[CustomMod]),
               scalaMetaToCustomTree(name).asInstanceOf[TermName],
               tparams.map(scalaMetaToCustomTree(_).asInstanceOf[TypeParam]),
               paramss.map(_.map(scalaMetaToCustomTree(_).asInstanceOf[TermParam])),
               decltpe.map(scalaMetaToCustomTree(_).asInstanceOf[CustomType]),
               scalaMetaToCustomTree(body).asInstanceOf[CustomTerm])

      case Defn.Enum(mods, name, tparams, ctor, templ) =>
        DefEnum(mods.map(scalaMetaToCustomTree(_).asInstanceOf[CustomMod]),
                scalaMetaToCustomTree(name).asInstanceOf[TypeName],
                tparams.map(scalaMetaToCustomTree(_).asInstanceOf[TypeParam]),
                scalaMetaToCustomTree(ctor).asInstanceOf[PrimaryCtor],
                scalaMetaToCustomTree(templ).asInstanceOf[CustomTemplate])

      case otherwise => throw new NotSupportedMetaTree(otherwise)
    }

    case scala.meta.Template(early, inits, self, stats) =>
      CustomTemplate(early.map(scalaMetaToCustomTree(_).asInstanceOf[CustomStat]),
                     inits.map(scalaMetaToCustomTree(_).asInstanceOf[CustomInit]),
                     scalaMetaToCustomTree(self).asInstanceOf[Self],
                     stats.map(scalaMetaToCustomTree(_).asInstanceOf[CustomStat]))

    case scala.meta.Source(stats) =>
      CustomSource(stats.map(scalaMetaToCustomTree(_).asInstanceOf[CustomStat]))

    case scala.meta.Pkg(ref, stats) =>
      CustomPkg(scalaMetaToCustomTree(ref).asInstanceOf[TermRef],
                stats.map(scalaMetaToCustomTree(_).asInstanceOf[CustomStat]))

    case scala.meta.Importer(ref, importees) =>
      Importer(scalaMetaToCustomTree(ref).asInstanceOf[TermRef],
               importees.map(scalaMetaToCustomTree(_).asInstanceOf[Importee]))

    case current: scala.meta.Importee => current match {
      case Importee.Wildcard() => ImporteeWildcard()
      case Importee.Given(tpe: CustomType) => ImporteeGiven(scalaMetaToCustomTree(tpe).asInstanceOf[CustomType])
      case Importee.GivenAll() => ImporteeGivenAll()
      case Importee.Name(name: CustomName) => ImporteeName(scalaMetaToCustomTree(name).asInstanceOf[CustomName])
      case otherwise => throw new NotSupportedMetaTree(otherwise)
    }

    case current: CustomPat => current match {
      case Pat.Var(name: Term.Name) => PatVar(scalaMetaToCustomTree(name).asInstanceOf[TermName])
      case otherwise => throw new NotSupportedMetaTree(otherwise)
    }

    case otherwise => throw new NotSupportedMetaTree(otherwise)
  }

  // do a pattern matching on (ctree: CustomTree)
  def customTreeToScalaMeta(ctree: CustomTree): scala.meta.Tree = ctree match {

    /* ============ CustomImport ============ */
    case Importer(ref: TermRef, importees: List[Importee]) =>
      scala.meta.Importer(customTreeToScalaMeta(ref).asInstanceOf[Term.Ref],
                          importees.map(customTreeToScalaMeta(_).asInstanceOf[scala.meta.Importee]))


    case CustomName(value: String) => ???
    case CustomInit(tpe: CustomTerm, name: CustomName, argss: List[List[CustomTerm]]) => ???

    case Self(name: CustomName, decltpe: Option[CustomType]) => ???

    case current: CustomType => current match {
      case TypeBounds(lo: Option[CustomType], hi: Option[CustomType]) => ???
      case TypeParam(mods: List[CustomMod],
                     name: CustomName,
                     tparams: List[TypeParam],
                     tbounds: TypeBounds,
                     vbounds: List[CustomType],
                     cbounds: List[CustomType]) => ???
      case TypeName(value: String) => ???
      case _ => throw new ThisMatchIsExhaustive()
    }

    case current: Enumerator => current match {
      case Generator(pat: CustomPat, rhs: CustomTerm) => ???
      case CaseGenerator(pat: CustomPat, rhs: CustomTerm) => ???
      case Val(pat: CustomPat, rhs: CustomTerm) => ???
      case Guard(cond: CustomTerm) => ???
    }


    /* ============ CustomMod ============ */
    case current: CustomMod => current match {
      case Private(within: Ref) => ???
      case Protected(within: Ref) => ???
      case Implicit() => ???
      case Abstract() => ???
      case Override() => ???
      case Super() => ???
      case Final() => ???
    }

    case current: CustomStat => current match {

      /* ============ CustomPkg ============ */
      case CustomPkg(ref: TermRef, stats: List[CustomStat]) => ???

      /* ============ CustomTerm ============ */
      case current: CustomTerm => current match {
        case TermName(value: String) => ???
        case TermThis(qual: CustomName) => ???
        case TermSuper(thisp: CustomName, superp: CustomName) => ???
        case TermParam(mods: List[CustomMod], name: CustomName,
                       decltpe: Option[CustomType], default: Option[CustomTerm]) => ???
        case TermLambda(params: List[TermParam], body: CustomTerm) => ???
        case TermSelect(qual: CustomTerm, name: TermName) => ???
        case TermInterpolate(prefix: TermName, parts: List[CustomLit],
                             args: List[CustomTerm]) => ???
        case TermApply(fun: CustomTerm, args: List[CustomTerm]) => ???
        case TermApplyUsing(fun: CustomTerm, args: List[CustomTerm]) => ???
        case TermApplyInfix(lhs: CustomTerm, op: TermName,
                            targs: List[CustomType], args: List[CustomTerm]) => ???
        case TermApplyUnary(op: TermName, arg: CustomTerm) => ???
        case TermAssign(lhs: CustomTerm, rhs: CustomTerm) => ???
        case TermReturn(expr: CustomTerm) => ???
        case TermNew(init: CustomInit) => ???
        case TermBlock(stats: List[CustomStat]) => ???
        case TermIf(cond: CustomTerm, thenp: CustomTerm, elsep: CustomTerm) => ???
        case TermTry(expr: CustomTerm, catchp: List[Case], finallyp: Option[CustomTerm]) => ???
        case TermWhile(expr: CustomTerm, body: CustomTerm) => ???
        case TermFor(enums: List[Enumerator], body: CustomTerm) => ???
        case TermThrow(expr: CustomTerm) => ???
        case current: TermRef => ???

        /* ============ CustomLit ============ */
        case current: CustomLit => current match {
          case LitInt(value) => ???
          case LitDouble(format) => ???
          case LitFloat(format) => ???
          case LitByte(value) => ???
          case LitShort(value) => ???
          case LitChar(value) => ???
          case LitLong(value) => ???
          case LitBoolean(value) => ???
          case LitUnit() => ???
          case LitString(value) => ???
          case LitSymbol(value) => ???
          case _ => throw new ThisMatchIsExhaustive()
        }

        case _ => throw new ThisMatchIsExhaustive()
      }

      /* ============ Defn ============ */
      case current: Defn => current match {
        case DefVal(mods: List[CustomMod], pats: List[CustomPat], decltpe: CustomType, rhs) => ???
        case DefVar(mods: List[CustomMod], pats: List[CustomPat], decltpe: CustomType, rhs) => ???
        case DefDef(mods: List[CustomMod], name: TermName,
                    tparams: List[TypeParam], params:List[List[TermParam]],
                    decltpe: CustomType, body: CustomTerm) => ???
        case DefEnum(mods: List[CustomMod], name: TypeName,
                     tparams: List[TypeParam], ctor: PrimaryCtor,
                     templ: CustomTemplate) => ???
        case _ => throw new ThisMatchIsExhaustive()
      }

      case CustomImport(importers: List[Importer]) => ???
    }


    /* ============ Ctor ============ */
    case current: Ctor => current match {
      case PrimaryCtor(mods: List[CustomMod], name: CustomName,
                       paramss: List[List[TermParam]]) => ???
      case SecondaryCtor(mods: List[CustomMod], name: CustomName,
                         paramss: List[List[TermParam]], init: CustomInit,
                         stats: List[CustomStat]) => ???

    }

    /* ============ CustomTemplate ============ */
    case CustomTemplate(early: List[CustomStat], inits: List[CustomInit],
                        self: Self, stats: List[CustomStat]) => ???

    /* ============ CustomSource ============ */
    case CustomSource(stats: List[CustomStat]) => ???

    case current: Importee => current match {
      case ImporteeWildcard() => ???
      case ImporteeGiven(tpe: CustomType) => ???
      case ImporteeGivenAll() => ???
      case ImporteeName(name: CustomName) => ???
    }

    /* ============ CustomPat ============ */
    case current: CustomPat => current match {
      case PatVar(name: TermName) => ???
      case _ => throw new ThisMatchIsExhaustive()
    }


    case _ => throw new ThisMatchIsExhaustive()
  }

}
