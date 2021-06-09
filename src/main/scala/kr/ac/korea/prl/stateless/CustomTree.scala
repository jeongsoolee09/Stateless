package kr.ac.korea.prl.stateless.CustomTree

/**
  * A custom lightweigh tree that provides a layer between scala.meta.CustomTree and jgrapht graphs
  * This focuses to serve simple Scala codes.
  *
  */

sealed trait CustomTree

/* ============ Ref ============ */
sealed trait Ref extends CustomTree

/* ============ Name ============ */
case class Name(value: String) extends Ref
case class Init(tpe: Term, name: Name, argss: List[List[Term]]) extends Ref

/* ============ Member ============ */
sealed trait Member extends CustomTree

/* ============ Member ============ */
case class Self(name: Name, decltpe: Option[Type]) extends Member

/* ============ Type ============ */
sealed trait Type extends CustomTree
case class TypeBounds(lo: Option[Type], hi: Option[Type]) extends Type
case class TypeParam(mods: List[Mod],
                     name: Name,
                     tparams: List[TypeParam],
                     tbounds: TypeBounds,
                     vbounds: List[Type],
                     cbounds: List[Type]) extends Type
case class TypeName(value: String)

/* ============ Stat ============ */
sealed trait Stat extends CustomTree

case class Case(pat: Pat, cond: Option[Term], body: Term) extends CustomTree

/* ============ Enumerator ============ */
sealed trait Enumerator extends CustomTree
class Generator(pat: Pat, rhs: Term) extends Enumerator
class CaseGenerator(pat: Pat, rhs: Term) extends Enumerator
class Val(pat: Pat, rhs: Term) extends Enumerator
class Guard(cond: Term) extends Enumerator

/* ============ Term ============ */
sealed trait Term extends Stat
case class TermName(value: String) extends Term
case class TermThis(qual: Name) extends Term
case class TermSuper(thisp: Name) extends Term
sealed trait TermRef extends Term with Ref
case class TermParam(mods: List[Mod], name: Name,
                     decltpe: Option[Type], default: Option[Term]) extends Term
case class TermLambda(params: List[TermParam], body: Term) extends Term
case class TermSelect(qual: Term, name: TermName) extends Term
case class TermInterpolate(prefix: Name, parts: List[Lit],
                           args: List[Term]) extends Term
case class TermApply(fun: Term, args: List[Term]) extends Term
case class TermApplyUsing(fun: Term, args: List[Term]) extends Term
case class TermApplyInfix(lhs: Term, op: Name,
                          targs: List[Type], args: List[Term]) extends Term
case class TermApplyUnary(op: Name, arg: Term) extends Term
case class TermAssign(lhs: Term, rhs: Term) extends Term
case class TermReturn(expr: Term) extends Term
case class TermNew(init: Init) extends Term
case class TermBlock(stats: List[Stat]) extends Term
case class TermIf(cond: Term, thenp: Term, elsep: Term) extends Term
case class TermTry(expr: Term, catchp: List[Case], finallyp: Option[Term]) extends Term
case class TermWhile(expr: Term, body: Term) extends Term
case class TermFor(enums: List[Enumerator], body: Term) extends Term
case class TermThrow(expr: Term) extends Term

/* ============ Lit ============ */
sealed trait Lit extends Term with Pat with Type
case class Int(value: Int) extends Lit
case class Double(format: Double) extends Lit
case class Float(format: Float) extends Lit
case class Byte(value: Byte) extends Lit
case class Short(value: Short) extends Lit
case class Char(value: Char) extends Lit
case class Long(value: Long) extends Lit
case class Boolean(value: Boolean) extends Lit
case class Unit() extends Lit
case class String(value: String) extends Lit
case class Symbol(value: Symbol) extends Lit

/* ============ Mod ============ */
sealed trait Mod extends CustomTree
case class Private(within: Ref) extends Mod
case class Protected(within: Ref) extends Mod
case class Implicit() extends Mod
case class Abstract() extends Mod
case class Override() extends Mod
case class Super() extends Mod
case class Final() extends Mod

/* ============ Defn ============ */
sealed trait Defn extends Stat
case class DefVal(mods: List[Mod], pats: List[Pat], decltpe: Type) extends Defn
case class DefVar(mods: List[Mod], pats: List[Pat], decltpe: Type) extends Defn
case class DefDef(mods: List[Mod], name: TermName,
                  tparams: List[TypeParam], params:List[List[TermParam]],
                  decltpe: Type) extends Defn
case class DefEnum(mods: List[Mod], name: TypeName,
                   tparams: List[TypeParam], ctor: Primary,
                   templ: Template) extends Defn

/* ============ Ctor ============ */
sealed trait Ctor extends CustomTree with Member
case class Primary(mods: List[Mod], name: Name,
                   paramss: List[List[TermParam]]) extends Ctor
case class Secondary(mods: List[Mod], name: Name,
                     paramss: List[List[TermParam]], init: Init,
                     stats: List[Stat]) extends Ctor

/* ============ Template ============ */
case class Template(early: List[Stat], inits: List[Init],
                    self: Self, stats: List[Stat]) extends CustomTree

/* ============ Source ============ */
case class Source(stats: List[Stat]) extends CustomTree

/* ============ Pkg ============ */
case class Pkg(ref: TermRef, stats: List[Stat]) extends Stat

/* ============ Import ============ */
sealed trait Importee extends CustomTree
case class Importer(ref: TermRef, importees: List[Importee]) extends CustomTree
case class ImporteeWildcard() extends Importee
case class ImporteeGiven(tpe: Type) extends Importee
case class ImporteeGivenAll() extends Importee
case class ImporteeName(name: Name) extends Importee

case class Import(importers: List[Importer]) extends Stat

/* ============ Pat ============ */
sealed trait Pat extends CustomTree
case class PatVar(name: TermName)
