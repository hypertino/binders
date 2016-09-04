package com.hypertino.binders

import com.hypertino.binders.value.internal.DynamicMacro

import scala.language.experimental.macros

package object value {
  implicit class ValueReader(val value: Value) {
    def fromValue[O]: O = macro DynamicMacro.fromValue[O]
  }

  implicit class ValueGenerator[O](val obj: O) {
    def toValue: Value = macro DynamicMacro.toValue[O]
  }

  implicit def int2number(i: Int): Number = Number(i)
  implicit def long2number(i: Long): Number = Number(i)
  implicit def bigdecimal2number(i: BigDecimal): Number = Number(i)
  implicit def double2number(i: Double): Number = Number(i)

  implicit def string2text(s: String): Text = Text(s)
  implicit def boolean2bool(b: Boolean): Bool = Bool(b)

  implicit def seq2lst(seq: Seq[Value]): Lst = Lst(seq)
  implicit def map2obj(map: Map[String, Value]): Obj = Obj(map)
}
