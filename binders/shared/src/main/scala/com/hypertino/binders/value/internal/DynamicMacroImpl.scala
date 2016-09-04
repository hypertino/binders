package com.hypertino.binders.value.internal

import scala.language.experimental.macros
import scala.language.reflectiveCalls
import scala.reflect.macros.Context

private [value] trait DynamicMacroImpl {
  val c: Context
  import c.universe._

  def fromValue[O: c.WeakTypeTag]: c.Tree = {
    val t = fresh("t")
    val d = fresh("s")
    val block = q"""{
      val $t = ${c.prefix.tree}
      ValueSerializerFactory.findFactory().withDeserializer[${weakTypeOf[O]}]($t.value) { case($d) => {
        $d.unbind[${weakTypeOf[O]}]
      }}
    }"""
    //println(block)
    block
  }

  def toValue[O: c.WeakTypeTag]: c.Tree = {
    val t = fresh("t")
    val s = fresh("s")
    val block = q"""{
      val $t = ${c.prefix.tree}
      ValueSerializerFactory.findFactory().withSerializer {case ($s) => {
        $s.bind[${weakTypeOf[O]}]($t.obj)
      }}
    }"""
    //println(block)
    block
  }

  def fresh(prefix: String): TermName = newTermName(c.fresh(prefix))
}
