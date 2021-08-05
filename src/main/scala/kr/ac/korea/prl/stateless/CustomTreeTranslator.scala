/**
  * Scala source code <=> ScalaMeta Tree <=> CustomTree <=> TreeGraph <=> SummarizedTreeGraph
  *                                      ^^^
  */


package kr.ac.korea.prl.stateless.CustomTreeTranslator


import scala.meta._

import kr.ac.korea.prl.stateless.CustomTree._

class NotSupportedMetaTree(tree: Tree) extends Exception
class ThisMatchIsExhaustive() extends Exception

object CustomTreeTranslator {

  /* I left the package qualifiers just for documentation */

  def scalaMetaToCustomTree(smtree: scala.meta.Tree): CustomTree = smtree match {

    case scala.meta.Init(tpe, name, argss) =>
      CustomInit(scalaMetaToCustomTree(tpe).asInstanceOf[CustomType],
                 scalaMetaToCustomTree(name).asInstanceOf[CustomName],
                 argss.map(_.map(scalaMetaToCustomTree(_).asInstanceOf[CustomTerm])))

    case scala.meta.Self(name, decltpe) =>
      CustomSelf(scalaMetaToCustomTree(name).asInstanceOf[CustomName],
                 decltpe.map(scalaMetaToCustomTree(_).asInstanceOf[CustomType]))

    case Type.Bounds(lo, hi) =>
      TypeBounds(lo.map(scalaMetaToCustomTree(_).asInstanceOf[CustomType]),
                 hi.map(scalaMetaToCustomTree(_).asInstanceOf[CustomType]))

    case Type.Param(mods, name, tparams,
                    tbounds, vbounds, cbounds) =>
      TypeParam(mods.map(scalaMetaToCustomTree(_).asInstanceOf[CustomMod]),
                scalaMetaToCustomTree(name).asInstanceOf[TypeName],
                tparams.map(scalaMetaToCustomTree(_).asInstanceOf[TypeParam]),
                scalaMetaToCustomTree(tbounds).asInstanceOf[TypeBounds],
                vbounds.map(scalaMetaToCustomTree(_).asInstanceOf[CustomType]),
                cbounds.map(scalaMetaToCustomTree(_).asInstanceOf[CustomType]))

    case Type.Name(value) => TypeName(value)

    case Type.Apply(tpe: Type, args: List[Type]) =>
      TypeApply(scalaMetaToCustomTree(tpe).asInstanceOf[CustomType],
                args.map(scalaMetaToCustomTree(_).asInstanceOf[CustomType]))

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

    case Term.Name(name: Predef.String) =>
      TermName(name)

    case Term.Param(mods, name, declpe, default) =>
      TermParam(mods.map(scalaMetaToCustomTree(_).asInstanceOf[CustomMod]),
                scalaMetaToCustomTree(name).asInstanceOf[TermName],
                declpe.map(scalaMetaToCustomTree(_).asInstanceOf[CustomType]),
                default.map(scalaMetaToCustomTree(_).asInstanceOf[CustomTerm]))

    case Term.Super(thisp, superp) =>
      TermSuper(scalaMetaToCustomTree(thisp).asInstanceOf[CustomName],
                scalaMetaToCustomTree(superp).asInstanceOf[CustomName])

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

    case Term.ApplyType(fun, targs) =>
      TermApplyType(scalaMetaToCustomTree(fun).asInstanceOf[CustomTerm],
                    targs.map(scalaMetaToCustomTree(_).asInstanceOf[CustomType]))

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
              catchp.map(scalaMetaToCustomTree(_).asInstanceOf[CustomCase]),
              finallyp.map(scalaMetaToCustomTree(_).asInstanceOf[CustomTerm]))

    case Term.While(expr, body) =>
      TermWhile(scalaMetaToCustomTree(expr).asInstanceOf[CustomTerm],
                scalaMetaToCustomTree(body).asInstanceOf[CustomTerm])

    case Term.For(enums, body) =>
      TermFor(enums.map(scalaMetaToCustomTree(_).asInstanceOf[CustomEnumerator]),
              scalaMetaToCustomTree(body).asInstanceOf[CustomTerm])

    case Term.Throw(expr) =>
      TermThrow(scalaMetaToCustomTree(expr).asInstanceOf[CustomTerm])

    case Lit.Null() => LitNull()

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

    case Mod.Private(within) => Private(scalaMetaToCustomTree(within).asInstanceOf[CustomRef])

    case Mod.Protected(within) => Protected(scalaMetaToCustomTree(within).asInstanceOf[CustomRef])

    case Mod.Implicit() => Implicit()

    case Mod.Abstract() => Abstract()

    case Mod.Override() => Override()

    case Mod.Super() => Super()

    case Mod.Final() => Final()

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

    case Defn.Class(mods, name, tparams, ctor, templ) =>
      DefClass(mods.map(scalaMetaToCustomTree(_).asInstanceOf[CustomMod]),
               scalaMetaToCustomTree(name).asInstanceOf[TypeName],
               tparams.map(scalaMetaToCustomTree(_).asInstanceOf[TypeParam]),
               scalaMetaToCustomTree(ctor).asInstanceOf[PrimaryCtor],
               scalaMetaToCustomTree(templ).asInstanceOf[CustomTemplate])

    case Defn.Object(mods, name, templ) =>
      DefObject(mods.map(scalaMetaToCustomTree(_).asInstanceOf[CustomMod]),
                scalaMetaToCustomTree(name).asInstanceOf[TermName],
                scalaMetaToCustomTree(templ).asInstanceOf[CustomTemplate])

    case Defn.Trait(mods, name, tparams, ctor, templ) =>
      DefTrait(mods.map(scalaMetaToCustomTree(_).asInstanceOf[CustomMod]),
               scalaMetaToCustomTree(name).asInstanceOf[TypeName],
               tparams.map(scalaMetaToCustomTree(_).asInstanceOf[TypeParam]),
               scalaMetaToCustomTree(ctor).asInstanceOf[PrimaryCtor],
               scalaMetaToCustomTree(templ).asInstanceOf[CustomTemplate])


    case scala.meta.Template(early, inits, self, stats) =>
      CustomTemplate(early.map(scalaMetaToCustomTree(_).asInstanceOf[CustomStat]),
                     inits.map(scalaMetaToCustomTree(_).asInstanceOf[CustomInit]),
                     scalaMetaToCustomTree(self).asInstanceOf[CustomSelf],
                     stats.map(scalaMetaToCustomTree(_).asInstanceOf[CustomStat]))

    case scala.meta.Source(stats) =>
      CustomSource(stats.map(scalaMetaToCustomTree(_).asInstanceOf[CustomStat]))

    case scala.meta.Pkg(ref, stats) =>
      CustomPkg(scalaMetaToCustomTree(ref).asInstanceOf[TermRef],
                stats.map(scalaMetaToCustomTree(_).asInstanceOf[CustomStat]))

    case scala.meta.Importer(ref, importees) =>
      CustomImporter(scalaMetaToCustomTree(ref).asInstanceOf[TermRef],
                     importees.map(scalaMetaToCustomTree(_).asInstanceOf[CustomImportee]))

    case Importee.Wildcard() => ImporteeWildcard()

    case Importee.Given(tpe: CustomType) => ImporteeGiven(scalaMetaToCustomTree(tpe).asInstanceOf[CustomType])

    case Importee.GivenAll() => ImporteeGivenAll()

    case Importee.Name(name: CustomName) => ImporteeName(scalaMetaToCustomTree(name).asInstanceOf[CustomName])

    case Pat.Var(name: Term.Name) => PatVar(scalaMetaToCustomTree(name).asInstanceOf[TermName])

    /* ============ Name ============ */
    case Name(value) => CustomName(value)

    case Ctor.Primary(mods: List[Mod], name: Name, paramss: List[List[Term.Param]]) =>
      PrimaryCtor(mods.map(scalaMetaToCustomTree(_).asInstanceOf[CustomMod]),
                  scalaMetaToCustomTree(name).asInstanceOf[CustomName],
                  paramss.map(_.map(scalaMetaToCustomTree(_).asInstanceOf[TermParam])))

    case Ctor.Secondary(mods: List[Mod], name: Name, paramss: List[List[Term.Param]],
                        init: Init, stats: List[Stat]) =>
      SecondaryCtor(mods.map(scalaMetaToCustomTree(_).asInstanceOf[CustomMod]),
                    scalaMetaToCustomTree(name).asInstanceOf[CustomName],
                    paramss.map(_.map(scalaMetaToCustomTree(_).asInstanceOf[TermParam])),
                    scalaMetaToCustomTree(init).asInstanceOf[CustomInit],
                    stats.map(scalaMetaToCustomTree(_).asInstanceOf[CustomStat]))

    case otherwise => {
      println(s"This is not supported: ${otherwise.toString}, which is: ${otherwise.getClass().toString()}")
      throw new NotSupportedMetaTree(otherwise)
    }
  }


  def customTreeToScalaMeta(ctree: CustomTree): scala.meta.Tree = ctree match {

    /* ============ CustomImport ============ */
    case CustomImporter(ref, importees ) =>
      scala.meta.Importer(customTreeToScalaMeta(ref).asInstanceOf[Term.Ref],
                          importees.map(customTreeToScalaMeta(_).asInstanceOf[scala.meta.Importee]))

    case CustomName(value) => ???

    case CustomInit(tpe, name, argss) => ???

    case Self(name, decltpe) => ???

    case current => current match {
      case TypeBounds(lo, hi ) => ???
      case TypeParam(mods,
                     name,
                     tparams,
                     tbounds,
                     vbounds,
                     cbounds ) => ???
      case TypeName(value) => ???
      case _ => throw new ThisMatchIsExhaustive()
    }

    case current => current match {
      case Generator(pat, rhs) => ???
      case CaseGenerator(pat, rhs) => ???
      case Val(pat, rhs) => ???
      case Guard(cond) => ???
    }


    /* ============ CustomMod ============ */
    case current => current match {
      case Private(within) => ???
      case Protected(within) => ???
      case Implicit() => ???
      case Abstract() => ???
      case Override() => ???
      case Super() => ???
      case Final() => ???
    }

    case current => current match {

      /* ============ CustomPkg ============ */
      case CustomPkg(ref, stats ) => ???

      /* ============ CustomTerm ============ */
      case current => current match {
        case TermName(value) => ???
        case TermThis(qual) => ???
        case TermSuper(thisp, superp) => ???
        case TermParam(mods, name,
                       decltpe, default ) => ???
        case TermLambda(params, body) => ???
        case TermSelect(qual, name) => ???
        case TermInterpolate(prefix, parts,
                             args ) => ???
        case TermApply(fun, args ) => ???
        case TermApplyUsing(fun, args ) => ???
        case TermApplyInfix(lhs, op,
                            targs, args ) => ???
        case TermApplyUnary(op, arg) => ???
        case TermAssign(lhs, rhs) => ???
        case TermReturn(expr) => ???
        case TermNew(init) => ???
        case TermBlock(stats ) => ???
        case TermIf(cond, thenp, elsep) => ???
        case TermTry(expr, catchp, finallyp ) => ???
        case TermWhile(expr, body) => ???
        case TermFor(enums, body) => ???
        case TermThrow(expr) => ???
        case current => ???

        /* ============ CustomLit ============ */
        case current => current match {
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
      case current => current match {
        case DefVal(mods, pats, decltpe, rhs) => ???
        case DefVar(mods, pats, decltpe, rhs) => ???
        case DefDef(mods, name,
                    tparams, params,
                    decltpe, body) => ???
        case DefEnum(mods, name,
                     tparams, ctor,
                     templ) => ???
        case _ => throw new ThisMatchIsExhaustive()
      }

      case CustomImport(importers ) => ???

      case _ => throw new ThisMatchIsExhaustive()
    }


    /* ============ Ctor ============ */
    case current => current match {
      case PrimaryCtor(mods, name,
                       paramss) => ???
      case SecondaryCtor(mods, name,
                         paramss, init,
                         stats ) => ???

    }

    /* ============ CustomTemplate ============ */
    case CustomTemplate(early, inits,
                        self, stats ) => ???

    /* ============ CustomSource ============ */
    case CustomSource(stats ) => ???

    case current => current match {
      case ImporteeWildcard() => ???
      case ImporteeGiven(tpe) => ???
      case ImporteeGivenAll() => ???
      case ImporteeName(name) => ???
    }

    /* ============ CustomPat ============ */
    case current => current match {
      case PatVar(name) => ???
      case _ => throw new ThisMatchIsExhaustive()
    }

    case _ => throw new ThisMatchIsExhaustive()
  }

}
