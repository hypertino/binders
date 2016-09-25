package com.hypertino.binders.value.internal

import com.hypertino.binders.util.MacroAdapter
import com.hypertino.binders.value.Value

import scala.language.experimental.macros
import MacroAdapter.Context

private [value] trait ValueCastMacroImpl extends MacroAdapter[Context] {
  import ctx.universe._

  // to
  def to[O: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[O]

    val block = if (tpe =:= typeOf[String]) {
      q"${ctx.prefix.tree}.value.toString"
    }
    else if (tpe =:= typeOf[Boolean]) {
      q"${ctx.prefix.tree}.value.toBoolean"
    }
    else if (tpe =:= typeOf[BigDecimal]) {
      q"${ctx.prefix.tree}.value.toBigDecimal"
    }
    else if (tpe =:= typeOf[Int]) {
      q"${ctx.prefix.tree}.value.toInt"
    }
    else if (tpe =:= typeOf[Long]) {
      q"${ctx.prefix.tree}.value.toLong"
    }
    else if (tpe =:= typeOf[Double]) {
      q"${ctx.prefix.tree}.value.toDouble"
    }
    else if (tpe =:= typeOf[Float]) {
      q"${ctx.prefix.tree}.value.toFloat"
    }
    else if (tpe =:= typeOf[Seq[Value]]) {
      q"${ctx.prefix.tree}.value.toSeq"
    }
    else if (tpe =:= typeOf[List[Value]]) {
      q"${ctx.prefix.tree}.value.toList"
    }
    else if (tpe =:= typeOf[Vector[Value]]) {
      q"${ctx.prefix.tree}.value.toVector"
    }
    else if (tpe =:= typeOf[Map[String, Value]]) {
      q"${ctx.prefix.tree}.value.toMap"
    }
    else {
      val t = freshTerm("t")
      val d = freshTerm("s")
      q"""{
      val $t = ${ctx.prefix.tree}
      com.hypertino.binders.value.ValueSerializerFactory.findFactory().withDeserializer[${weakTypeOf[O]}]($t.value) { case($d) => {
        $d.unbind[${weakTypeOf[O]}]
      }}
    }"""
    }
    //println(block)
    block
  }

  def toValue[O: ctx.WeakTypeTag]: ctx.Tree = {
    val t = freshTerm("t")
    val s = freshTerm("s")
    val block = q"""{
      val $t = ${ctx.prefix.tree}
      com.hypertino.binders.value.ValueSerializerFactory.findFactory().withSerializer {case ($s) => {
        $s.bind[${weakTypeOf[O]}]($t.obj)
      }}
    }"""
    //println(block)
    block
  }
}
