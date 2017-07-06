package com.hypertino.binders

import com.hypertino.binders.value.internal.ValueCastMacro
import scala.language.experimental.macros
import scala.language.implicitConversions

package object value {
  implicit class ValueReader(val value: Value) extends AnyVal {
    def to[O]: O = macro ValueCastMacro.to[O]
  }

  implicit class ValueGenerator[O](val obj: O) extends AnyVal {
    def toValue: Value = macro ValueCastMacro.toValue[O]
  }

  implicit def int2number(i: Int): Number = Number(i)
  implicit def oint2value(i: Option[Int]): Value = i.map(Number(_)).getOrElse(Null)
  implicit def long2number(i: Long): Number = Number(i)
  implicit def olong2value(i: Option[Long]): Value = i.map(Number(_)).getOrElse(Null)
  implicit def bigdecimal2number(i: BigDecimal): Number = Number(i)
  implicit def obigdecimal2value(i: Option[BigDecimal]): Value = i.map(Number).getOrElse(Null)
  implicit def double2number(i: Double): Number = Number(i)
  implicit def odouble2value(i: Option[Double]): Value = i.map(Number(_)).getOrElse(Null)

  implicit def string2text(s: String): Text = Text(s)
  implicit def ostring2text(s: Option[String]): Value = s.map(Text).getOrElse(Null)
  implicit def boolean2bool(b: Boolean): Bool = Bool(b)
  implicit def oboolean2bool(b: Option[Boolean]): Value = b.map(Bool(_)).getOrElse(Null)

  implicit def seq2lst(seq: Seq[Value]): Lst = Lst(seq)
  implicit def oseq2lst(seq: Option[Seq[Value]]): Value = seq.map(Lst(_)).getOrElse(Null)

  implicit def map2obj(map: Map[String, Value]): Obj = Obj(map)
  implicit def omap2obj(map: Option[Map[String, Value]]): Value = map.map(Obj(_)).getOrElse(Null)
}
