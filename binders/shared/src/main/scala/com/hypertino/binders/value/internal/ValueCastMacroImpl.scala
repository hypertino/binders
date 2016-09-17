package com.hypertino.binders.value.internal

import com.hypertino.binders.value.Value

import scala.language.experimental.macros
import scala.language.reflectiveCalls
import scala.reflect.macros.Context

private [value] trait ValueCastMacroImpl {
  val c: Context
  import c.universe._

  // to
  def to[O: c.WeakTypeTag]: c.Tree = {
    val tpe = weakTypeOf[O]

    val block = if (tpe =:= typeOf[String]) {
      q"${c.prefix.tree}.value.toString"
    }
    else if (tpe =:= typeOf[Boolean]) {
      q"${c.prefix.tree}.value.toBoolean"
    }
    else if (tpe =:= typeOf[BigDecimal]) {
      q"${c.prefix.tree}.value.toBigDecimal"
    }
    else if (tpe =:= typeOf[Int]) {
      q"${c.prefix.tree}.value.toInt"
    }
    else if (tpe =:= typeOf[Long]) {
      q"${c.prefix.tree}.value.toLong"
    }
    else if (tpe =:= typeOf[Double]) {
      q"${c.prefix.tree}.value.toDouble"
    }
    else if (tpe =:= typeOf[Float]) {
      q"${c.prefix.tree}.value.toFloat"
    }
    else if (tpe =:= typeOf[Seq[Value]]) {
      q"${c.prefix.tree}.value.toSeq"
    }
    else if (tpe =:= typeOf[List[Value]]) {
      q"${c.prefix.tree}.value.toList"
    }
    else if (tpe =:= typeOf[Vector[Value]]) {
      q"${c.prefix.tree}.value.toVector"
    }
    else if (tpe =:= typeOf[Map[String, Value]]) {
      q"${c.prefix.tree}.value.toMap"
    }
    else {
      val t = fresh("t")
      val d = fresh("s")
      q"""{
      val $t = ${c.prefix.tree}
      com.hypertino.binders.value.ValueSerializerFactory.findFactory().withDeserializer[${weakTypeOf[O]}]($t.value) { case($d) => {
        $d.unbind[${weakTypeOf[O]}]
      }}
    }"""
    }
    //println(block)
    block
  }

  def toValue[O: c.WeakTypeTag]: c.Tree = {
    val t = fresh("t")
    val s = fresh("s")
    val block = q"""{
      val $t = ${c.prefix.tree}
      com.hypertino.binders.value.ValueSerializerFactory.findFactory().withSerializer {case ($s) => {
        $s.bind[${weakTypeOf[O]}]($t.obj)
      }}
    }"""
    //println(block)
    block
  }

  def fresh(prefix: String): TermName = newTermName(c.fresh(prefix))
}
