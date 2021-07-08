/**
  * Scala source code <=> ScalaMeta Tree <=> CustomTree <=> TreeGraph <=> SummarizedTreeGraph
  *                                          ^^^^^^^^^^
  */


package kr.ac.korea.prl.stateless.CustomTree

/**
  * A custom lightweight tree that provides a layer between scala.meta.Tree and jgrapht graphs
  * This focuses to serve simple Scala codes.
  *
  */

import scala.meta._

sealed trait CustomTree


/* ============ CustomRef ============ */
sealed trait CustomRef extends CustomTree


/* ============ CustomName ============ */
case class CustomName(value: String) extends CustomRef
case class CustomInit(tpe: CustomType,
                      name: CustomName,
                      argss: List[List[CustomTerm]]) extends CustomRef


/* ============ Member ============ */
sealed trait Member extends CustomTree


/* ============ Member ============ */
case class CustomSelf(name: CustomName,
                      decltpe: Option[CustomType]) extends Member {
  def apply(name: CustomName,
            decltpe: Option[CustomType]) = new CustomSelf(name,
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
                     name: TypeName, // intentionally more specific than CustomName
                     tparams: List[TypeParam],
                     tbounds: TypeBounds,
                     vbounds: List[CustomType],
                     cbounds: List[CustomType]) extends CustomType {
  def apply(mods: List[CustomMod],
            name: TypeName,
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

case class TypeApply(tpe: CustomType, arg: List[CustomType]) extends CustomType {
  def apply(tpe: CustomType, arg: List[CustomType]) = new TypeApply(tpe, arg)
}

/* ============ CustomStat ============ */
sealed trait CustomStat extends CustomTree

case class CustomCase(pat: CustomPat,
                      cond: Option[CustomTerm],
                      body: CustomTerm) extends CustomTree {
  def apply(pat: CustomPat,
            cond: Option[CustomTerm],
            body: CustomTerm) = new CustomCase(pat,
                                               cond,
                                               body)
}


/* ============ CustomEnumerator ============ */
sealed trait CustomEnumerator extends CustomTree
case class Generator(pat: CustomPat,
                     rhs: CustomTerm) extends CustomEnumerator {
  def apply(pat: CustomPat,
            rhs: CustomTerm) = new Generator(pat,
                                             rhs)
}

case class CaseGenerator(pat: CustomPat,
                         rhs: CustomTerm) extends CustomEnumerator {
  def apply(pat: CustomPat,
            rhs: CustomTerm) = new CaseGenerator(pat,
                                                 rhs)
}

case class Val(pat: CustomPat,
               rhs: CustomTerm) extends CustomEnumerator {
  def apply(pat: CustomPat,
            rhs: CustomTerm) = new Val(pat,
                                       rhs)
}

case class Guard(cond: CustomTerm) extends CustomEnumerator {
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

sealed trait TermRef extends CustomTerm with CustomRef
case class TermParam(mods: List[CustomMod],
                     name: TermName, // Intentionally more specific than CustomName
                     decltpe: Option[CustomType],
                     default: Option[CustomTerm]) extends CustomTerm {
  def apply(mods: List[CustomMod],
            name: TermName,
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

case class TermApplyType(fun: CustomTerm,
                         targs: List[CustomType]) extends CustomTerm {
  def apply(fun: CustomTerm,
            targs: List[CustomType]) = new TermApplyType(fun,
                                                         targs)
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
                   catchp: List[CustomCase],
                   finallyp: Option[CustomTerm]) extends CustomTerm {
  def apply(expr: CustomTerm,
            catchp: List[CustomCase],
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

case class TermFor(enums: List[CustomEnumerator],
                   body: CustomTerm) extends CustomTerm {
  def apply(enums: List[CustomEnumerator],
            body: CustomTerm) = new TermFor(enums,
                                            body)
}

case class TermThrow(expr: CustomTerm) extends CustomTerm {
  def apply(expr: CustomTerm) = new TermThrow(expr)
}


/* ============ CustomLit ============ */
sealed trait CustomLit extends CustomTerm with CustomPat with CustomType

case class LitNull() extends CustomLit {
  def apply() = new LitNull()
}

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
case class Private(within: CustomRef) extends CustomMod {
  def apply(within: CustomRef) = new Private(within)
}

case class Protected(within: CustomRef) extends CustomMod {
  def apply(within: CustomRef) = new Protected(within)
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
case class DeclDef(mods: List[CustomMod],
                   name: TermName,
                   tparams: List[TypeParam],
                   paramss: List[List[TermParam]],
                   decltpe: CustomType) extends Decl


/* ============ CustomDefn ============ */
sealed trait CustomDefn extends CustomStat
case class DefVal(mods: List[CustomMod],
                  pats: List[CustomPat],
                  decltpe: Option[CustomType],
                  rhs: CustomTerm) extends CustomDefn {
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
                  rhs: Option[CustomTerm]) extends CustomDefn {
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
                  body: CustomTerm) extends CustomDefn {
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
                   templ: CustomTemplate) extends CustomDefn {
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
                    templ: CustomTemplate) extends CustomDefn {
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
                     name: TermName,
                     templ: CustomTemplate) extends CustomDefn {
  def apply(mods: List[CustomMod],
            name: TermName,
            templ: CustomTemplate) = new DefObject(mods,
                                                   name,
                                                   templ)
}

case class DefTrait(mods: List[CustomMod],
                    name: TypeName,
                    tparams: List[TypeParam],
                    ctor: PrimaryCtor,
                    templ: CustomTemplate) extends CustomDefn


/* ============ CustomCtor ============ */
sealed trait CustomCtor extends CustomTree with Member
case class PrimaryCtor(mods: List[CustomMod],
                       name: CustomName,
                       paramss: List[List[TermParam]]) extends CustomCtor {
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
                         stats: List[CustomStat]) extends CustomCtor {
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
                          self: CustomSelf,
                          stats: List[CustomStat]) extends CustomTree {
  def apply(early: List[CustomStat],
            inits: List[CustomInit],
            self: CustomSelf,
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
sealed trait CustomImportee extends CustomTree
case class CustomImporter(ref: TermRef,
                          importees: List[CustomImportee]) extends CustomTree {
  def apply(ref: TermRef,
            importees: List[CustomImportee]) = new CustomImporter(ref,
                                                                  importees)
}

case class ImporteeWildcard() extends CustomImportee {
  def apply() = new ImporteeWildcard()
}

case class ImporteeGiven(tpe: CustomType) extends CustomImportee {
  def apply(tpe: CustomType) = new ImporteeGiven(tpe)
}

case class ImporteeGivenAll() extends CustomImportee {
  def apply() = new ImporteeGivenAll()
}

case class ImporteeName(name: CustomName) extends CustomImportee {
  def apply(name: CustomName) = new ImporteeName(name)
}

case class CustomImport(importers: List[CustomImporter]) extends CustomStat {
  def apply(importers: List[CustomImporter]) = new CustomImport(importers)
}


/* ============ ASTList ============ */
// the "id" slot keeps each instances distinct in a VertexSet.
case class ASTList(id: Int) extends CustomTree
case class ASTSome(id: Int) extends CustomTree
case class ASTNone(id: Int) extends CustomTree

