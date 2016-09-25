package com.hypertino.binders.util

trait MacroAdapter[C <: MacroAdapter.Context] {
  val ctx: C
  import ctx.universe._

  def freshName(prefix: String) = ctx.freshName(prefix)
  def freshTerm(prefix: String): TermName = TermName(freshName(prefix))
}

object MacroAdapter {
  type Context = scala.reflect.macros.blackbox.Context
}
