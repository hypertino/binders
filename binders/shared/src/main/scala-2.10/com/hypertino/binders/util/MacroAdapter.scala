package com.hypertino.binders.util

import scala.language.implicitConversions

trait MacroAdapter[C <: MacroAdapter.Context] {
  val ctx: C
  import ctx.universe._

  val noSelfType = ValDef(Modifiers(Flag.PRIVATE), nme.WILDCARD, TypeTree(NoType), EmptyTree)
  val termNames = nme
  val typeNames = tpnme

  def TermName(s: String) = newTermName(s)
  def TypeName(s: String) = newTypeName(s)
  def freshName(prefix: String) = ctx.fresh(prefix)
  def freshTerm(prefix: String): TermName = TermName(freshName(prefix))

  implicit class TypeExtenders(t: Type) {
    def typeParams:List[Symbol] = {
      t match  {
        case PolyType(res, _) => res
        case _ => List.empty
      }
    }

    def typeArgs: List[Type] = {
      t match {
        case TypeRef(typ, tsym, typeParams) ⇒
          typeParams

        case _ ⇒
          List.empty
      }
    }

    def dealias:Type = t.normalize
    def resultType: Type = t
  }

  implicit class MethodExtenders(m: MethodSymbolApi) {
    def paramLists = m.paramss
  }
}

object MacroAdapter {
  type Context = scala.reflect.macros.Context
}
