package com.hypertino.binders.value

import scala.language.dynamics
import scala.language.experimental.macros
import ValueBinders._

sealed trait Value extends Any with Dynamic {
  def ~~[T](visitor: ValueVisitor[T]): T

  override def toString: String = this ~~ Visitors.toStringVisitor

  def toBoolean: Boolean = this ~~ Visitors.toBooleanVisitor

  def toBigDecimal: BigDecimal = this ~~ Visitors.toBigDecimalVisitor

  def toInt: Int = toBigDecimal.toIntExact
  def toLong: Long = toBigDecimal.toLongExact
  def toDouble: Double = toBigDecimal.toDouble
  def toFloat: Float = toBigDecimal.toFloat

  def toMap: scala.collection.Map[String, Value] = this ~~ Visitors.toMapVisitor

  def toSeq: Seq[Value] = this ~~ Visitors.toSeqVisitor

  def toList: List[Value] = toSeq.toList

  def toVector: Vector[Value] = toSeq.toVector

  def isDefined: Boolean = !isNull

  def isNull: Boolean = this ~~ Visitors.isNullVisitor

  def isEmpty: Boolean = this ~~ Visitors.isEmptyVisitor

  def +(other: Value): Value = throw new UnsupportedOperationException(s"$this + $other")
  def -(other: Value): Value = throw new UnsupportedOperationException(s"$this - $other")
  def ++(other: Value): Value = throw new UnsupportedOperationException(s"$this ++ $other")
  def --(other: Value): Value = throw new UnsupportedOperationException(s"$this -- $other")
  def *(other: Value): Value = throw new UnsupportedOperationException(s"$this * $other")
  def /(other: Value): Value = throw new UnsupportedOperationException(s"$this / $other")
  def %(other: Value): Value = throw new UnsupportedOperationException(s"$this % $other")
  def |(other: Value): Value = throw new UnsupportedOperationException(s"$this | $other")
  def ^(other: Value): Value = throw new UnsupportedOperationException(s"$this ^ $other")
  def &(other: Value): Value = throw new UnsupportedOperationException(s"$this & $other")
  def >(other: Value): Boolean = throw new UnsupportedOperationException(s"$this > $other")
  def <(other: Value): Boolean = throw new UnsupportedOperationException(s"$this < $other")
  def >=(other: Value): Boolean = throw new UnsupportedOperationException(s"$this >= $other")
  def <=(other: Value): Boolean = throw new UnsupportedOperationException(s"$this <= $other")
  def contains(other: Value): Boolean = throw new UnsupportedOperationException(s"$this contains $other")
  def unary_! : Value = throw new UnsupportedOperationException(s"!$this")
  def unary_- : Value = throw new UnsupportedOperationException(s"-$this")

  def selectDynamic(name: String): Value = {
    toMap.getOrElse(
      if (name.startsWith("_") && name.length > 1) {
        name.substring(1)
      } else {
        name
      },
      Null
    )
  }
}

trait ValueVisitor[T] {
  def visitNumber(d: Number): T
  def visitText(d: Text): T
  def visitObj(d: Obj): T
  def visitLst(d: Lst): T
  def visitBool(d: Bool): T
  def visitNull(): T
}

case object Null extends Value {
  override def ~~[T](visitor: ValueVisitor[T]): T = visitor.visitNull()

  override def +(other: Value): Value = Null
  override def -(other: Value): Value = Null
  override def *(other: Value): Value = Null
  override def /(other: Value): Value = Null
  override def %(other: Value): Value = Null
  override def >(other: Value): Boolean = false
  override def <(other: Value): Boolean = false
  override def >=(other: Value): Boolean = other == Null
  override def <=(other: Value): Boolean = other == Null
  override def contains(other: Value): Boolean = false
  override def unary_! : Value = Null
  override def unary_- : Value = Null
}

case class Number(v: BigDecimal) extends AnyVal with Value {
  override def ~~[T](visitor: ValueVisitor[T]): T = visitor.visitNumber(this)

  override def +(other: Value): Value = if (other != Null)
    v + other.toBigDecimal
  else
    Null

  override def -(other: Value): Value = if (other != Null)
    v - other.toBigDecimal
  else
    Null

  override def *(other: Value): Value = if (other != Null)
    v * other.toBigDecimal
  else
    Null

  override def /(other: Value): Value = if (other != Null)
    v / other.toBigDecimal
  else
    Null

  override def %(other: Value): Value = if (other != Null)
    v % other.toBigDecimal
  else
    Null

  override def |(other: Value): Value = if (other != Null)
    BigDecimal(v.toBigInt() | other.toBigDecimal.toBigInt())
  else
    Null

  override def &(other: Value): Value = if (other != Null)
    BigDecimal(v.toBigInt() & other.toBigDecimal.toBigInt())
  else
    Null

  override def ^(other: Value): Value = if (other != Null)
    BigDecimal(v.toBigInt() ^ other.toBigDecimal.toBigInt())
  else
    Null

  override def >(other: Value): Boolean = if (other != Null)
    v > other.toBigDecimal
  else
    false

  override def <(other: Value): Boolean = if (other != Null)
    v < other.toBigDecimal
  else
    false

  override def >=(other: Value): Boolean = if (other != Null)
    v >= other.toBigDecimal
  else
    false

  override def <=(other: Value): Boolean = if (other != Null)
    v <= other.toBigDecimal
  else
    false

  override def unary_! : Value = BigDecimal(~v.toBigInt())
  override def unary_- : Value = -v
}

case class Text(v: String) extends AnyVal with Value {
  override def ~~[T](visitor: ValueVisitor[T]): T = visitor.visitText(this)
  override def +(other: Value): Value = v + other.toString
  override def >(other: Value): Boolean = v > other.toString
  override def <(other: Value): Boolean = v < other.toString
  override def >=(other: Value): Boolean = v >= other.toString
  override def <=(other: Value): Boolean = v <= other.toString
  override def contains(other: Value): Boolean = v.contains(other.toString)
}

case class Obj(v: scala.collection.Map[String, Value]) extends AnyVal with Value{
  override def ~~[T](visitor: ValueVisitor[T]): T = visitor.visitObj(this)

  override def +(other: Value): Value = {
    other match {
      case o: Obj ⇒
        Obj(v ++ o.v.map {
          case (k, otherV) => k -> v.get(k).map { originalV ⇒
            if (originalV.isInstanceOf[Obj]) {
              originalV.+(otherV)
            }
            else {
              otherV
            }
          }.getOrElse {
            otherV
          }
        })
      case _ ⇒
        super.+(other)
    }
  }

  override def -(other: Value): Value = {
    Obj(v.filterNot(kv ⇒ other.toMap.contains(kv._1)))
  }

  override def contains(other: Value): Boolean = v.contains(other.toString)
}

object Obj {
  val empty = new Obj(Map.empty)
  def apply(): Obj = empty
}

object ObjV {
  // currently there is only mutable effective map that preserves order
  // so we use LinkedHashMap for that
  def apply(v: (String,Value)*): Obj = {
    if (v.isEmpty)
      Obj.empty
    else
      new Obj(scala.collection.mutable.LinkedHashMap(v: _*))
  }
}

case class Lst(v: Seq[Value]) extends AnyVal with Value{
  override def ~~[T](visitor: ValueVisitor[T]): T = visitor.visitLst(this)

  override def +(other: Value): Value = v :+ other
  override def -(other: Value): Value = {
    v.diff(Seq(other))
  }

  override def ++(other: Value): Value = {
    other match {
      case Lst(seqOther) ⇒ v ++ seqOther
      case _ ⇒ super.++(other)
    }
  }

  override def --(other: Value): Value = {
    other match {
      case Lst(seqOther) ⇒ v diff seqOther
      case _ ⇒ super.--(other)
    }
  }

  override def contains(other: Value): Boolean = v.contains(other)
}

object Lst {
  val empty = Lst(Seq.empty)
  def apply(): Lst = empty
}

object LstV { // can't be Lst :-(
  def apply(seq: Value*): Lst = {
    if (seq.isEmpty)
      Lst.empty
    else
      Lst(seq)
  }
}

trait Bool extends Value with Product {
  val v: Boolean
  override def ~~[T](visitor: ValueVisitor[T]): T = visitor.visitBool(this)

  override def |(other: Value): Value = v || other.toBoolean
  override def &(other: Value): Value = v && other.toBoolean
  override def ^(other: Value): Value = v ^ other.toBoolean
  override def >(other: Value): Boolean = v > other.toBoolean
  override def <(other: Value): Boolean = v < other.toBoolean
  override def >=(other: Value): Boolean = v >= other.toBoolean
  override def <=(other: Value): Boolean = v <= other.toBoolean
  override def unary_! : Value = !v

  // product methods
  def copy(o: Boolean): Bool
  override def equals(obj: scala.Any): Boolean = obj match {
    case null ⇒ false
    case Bool(o) ⇒ v == o
    case b: Boolean ⇒ v == b
    case _ ⇒ false
  }
  override def hashCode(): Int = if(v) 1 else 0
  override def toString: String = s"Bool($v)"
  override def productArity: Int = 1
  override def productElement(n: Int): Any = if (n == 0) v else throw new IndexOutOfBoundsException(s"$n is out of bounds")
}

object Bool {
  def apply(v: Boolean): Bool = if(v) True else False
  def unapply(b: Bool): Option[Boolean] = Some(b.v)
}

case object True extends Bool {
  val v: Boolean = true
  def copy(o: Boolean) = Bool(o)
}

case object False extends Bool {
  val v: Boolean = false
  def copy(o: Boolean) = Bool(o)
}

private [value] object Visitors {
  val toStringVisitor = new ValueVisitor[String] {
    override def visitBool(d: Bool) = d.v.toString
    override def visitText(d: Text) = d.v
    override def visitObj(d: Obj) = d.v.map(kv => kv._1 + "->" + kv._2).mkString(",")
    override def visitNumber(d: Number) = d.v.toString()
    override def visitLst(d: Lst) = d.v.mkString(",")
    override def visitNull(): String = ""
  }

  val toBooleanVisitor = new ValueVisitor[Boolean] {
    override def visitBool(d: Bool) = d.v
    override def visitText(d: Text) = d.v.toLowerCase match {
      case "true" => true
      case "y" => true
      case "yes" => true
      case "on" => true
      case "1" => true
      case "false" => false
      case "n" => false
      case "no" => false
      case "off" => false
      case "0" => false
      case _ => castUnavailable(s"String(${d.v}) to Boolean")
    }
    override def visitObj(d: Obj) = castUnavailable("Obj to Boolean")
    override def visitNumber(d: Number) = d.v != BigDecimal(0)
    override def visitLst(d: Lst) = castUnavailable("Lst to Boolean")
    override def visitNull(): Boolean = false
  }

  val toBigDecimalVisitor = new ValueVisitor[BigDecimal] {
    override def visitBool(d: Bool) = if(d.v) 1 else 0
    override def visitText(d: Text) = BigDecimal(d.v)
    override def visitObj(d: Obj) = castUnavailable("Obj to BigDecimal")
    override def visitNumber(d: Number) = d.v
    override def visitLst(d: Lst) = castUnavailable("Lst to BigDecimal")
    override def visitNull(): BigDecimal = 0
  }

  val toMapVisitor = new ValueVisitor[scala.collection.Map[String, Value]] {
    override def visitBool(d: Bool) = castUnavailable("Bool to Map[]")
    override def visitText(d: Text) = castUnavailable("Text to Map[]")
    override def visitObj(d: Obj) = d.v
    override def visitNumber(d: Number) = castUnavailable("Number to Map[]")
    override def visitLst(d: Lst) = castUnavailable("Lst to Map[]")
    override def visitNull() = Map.empty
  }

  val toSeqVisitor = new ValueVisitor[Seq[Value]] {
    override def visitBool(d: Bool) = castUnavailable("Bool to Seq[]")
    override def visitText(d: Text) = castUnavailable("Text to Seq[]")
    override def visitObj(d: Obj) = castUnavailable("Obj to Seq[]")
    override def visitNumber(d: Number) = castUnavailable("Number to Seq[]")
    override def visitLst(d: Lst) = d.v
    override def visitNull() = Seq()
  }

  val isNullVisitor = new ValueVisitor[Boolean] {
    override def visitBool(d: Bool) = false
    override def visitText(d: Text) = false
    override def visitObj(d: Obj) = false
    override def visitNumber(d: Number) = false
    override def visitLst(d: Lst) = false
    override def visitNull() = true
  }

  val isEmptyVisitor = new ValueVisitor[Boolean] {
    override def visitBool(d: Bool) = false
    override def visitText(d: Text) = d.v.isEmpty
    override def visitObj(d: Obj) = d.v.isEmpty
    override def visitNumber(d: Number) = false
    override def visitLst(d: Lst) = d.v.isEmpty
    override def visitNull() = true
  }

  def castUnavailable(s: String) = throw new ClassCastException(s)
}
