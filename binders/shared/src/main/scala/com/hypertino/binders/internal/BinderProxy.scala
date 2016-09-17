package com.hypertino.binders.internal

import scala.language.reflectiveCalls
import scala.reflect.macros.Context
import language.experimental.macros

private [binders] object BinderProxy {

  def bind[S: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context)
  (value: c.Expr[O]): c.Expr[S] = {

    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with BindersMacroImpl
    c.Expr[S](bundle.bind[S, O](value.tree))
  }

  def bindArgs[S: c.WeakTypeTag]
  (c: Context)
  (t: c.Expr[S]*): c.Expr[S] = {

    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with BindersMacroImpl
    c.Expr[S](bundle.bindArgs[S](t.map(_.tree)))
  }

  def bindPartial[S: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context)
  (value: c.Expr[O]): c.Expr[S] = {

    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with BindersMacroImpl
    c.Expr[S](bundle.bindObject[S, O](value.tree, true))
  }

  def unbind[D: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context): c.Expr[O] = {

    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with BindersMacroImpl
    c.Expr[O](bundle.unbind[D, O](false, null))
  }

  def unbindPartial[D: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context)
  (originalValue: c.Expr[O]): c.Expr[O] = {

    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with BindersMacroImpl
    c.Expr[O](bundle.unbind[D, O](true, originalValue.tree))
  }
}
