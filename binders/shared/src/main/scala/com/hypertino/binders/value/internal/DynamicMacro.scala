package com.hypertino.binders.value.internal

import com.hypertino.binders.value.Value

import scala.language.experimental.macros
import scala.language.reflectiveCalls
import scala.reflect.macros.Context

private [value] object DynamicMacro {
  def fromValue[O: c.WeakTypeTag]
  (c: Context): c.Expr[O] = {
    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with DynamicMacroImpl
    c.Expr[O](bundle.fromValue[O])
  }

  def toValue[O: c.WeakTypeTag]
  (c: Context): c.Expr[Value] = {
    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with DynamicMacroImpl
    c.Expr[Value](bundle.toValue[O])
  }
}
