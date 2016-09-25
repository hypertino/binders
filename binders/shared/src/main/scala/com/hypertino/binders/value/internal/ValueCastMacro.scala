package com.hypertino.binders.value.internal

import com.hypertino.binders.value.Value

import scala.language.experimental.macros
import com.hypertino.binders.util.MacroAdapter.Context

private [value] object ValueCastMacro {
  def to[O: c.WeakTypeTag]
  (c: Context): c.Expr[O] = {
    val c0: c.type = c
    val bundle = new {
      val ctx: c0.type = c0
    } with ValueCastMacroImpl
    c.Expr[O](bundle.to[O])
  }

  def toValue[O: c.WeakTypeTag]
  (c: Context): c.Expr[Value] = {
    val c0: c.type = c
    val bundle = new {
      val ctx: c0.type = c0
    } with ValueCastMacroImpl
    c.Expr[Value](bundle.toValue[O])
  }
}
