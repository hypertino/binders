package com.hypertino.binders.util

trait MacroAdapter[C <: MacroAdapter.Context] {
  val ctx: C
  import ctx.universe._

  def freshName(prefix: String) = ctx.freshName(prefix)
  def freshTerm(prefix: String): TermName = TermName(freshName(prefix))

  implicit class AnnotationExtenders(a: AnnotationApi) {
    def arguments = a.tree.children.tail
    def treeTpe = a.tree.tpe
  }
}

object MacroAdapter {
  type Context = scala.reflect.macros.blackbox.Context
}
