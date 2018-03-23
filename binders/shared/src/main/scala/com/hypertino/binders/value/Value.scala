package com.hypertino.binders.value

import scala.language.experimental.macros
import scala.language.dynamics

sealed trait Value extends Any {
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

  def nonEmpty: Boolean = !isEmpty
  def mkString(sep: String): String = mkString("",sep,"",None)
  def mkString(start: String, sep: String, end: String): String =mkString(start,sep,end,None)
  def mkString(alternative: Option[String]): String =mkString("","","",alternative)
  def mkString(start: String, sep: String, end: String, alternative: Option[String]): String = {
    if (isEmpty && alternative.isDefined) {
      alternative.get
    }
    else {
      start + toString + end
    }
  }
  def getOrElse(default: ⇒ Value): Value = {
    if (isNull) default else this
  }
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
  def apply(other: Value): Value = throw new UnsupportedOperationException(s"$this.apply($other)")

  def dynamic: ValueDynamicSelector = ValueDynamicSelector(this)
}

trait ValueVisitor[T] {
  def visitNumber(d: Number): T
  def visitText(d: Text): T
  def visitObj(d: Obj): T
  def visitLst(d: Lst): T
  def visitBool(d: Bool): T
  def visitNull(): T
}

object Value {
  def removeNullFields(v: Value): Value = {
    v ~~ Visitors.removeNullFieldsVisitor
  }
}

case object Null extends Value {
  override def ~~[T](visitor: ValueVisitor[T]): T = visitor.visitNull()

  override def +(other: Value): Value = other match {
    case obj: Obj ⇒ obj
    case _ ⇒ Null
  }
  override def %(other: Value): Value = other match {
    case obj: Obj ⇒ obj
    case _ ⇒ Null
  }
  override def ++(other: Value): Value = other match {
    case lst: Lst ⇒ lst
    case _ ⇒ Null
  }
  override def --(other: Value): Value = Null
  override def -(other: Value): Value = Null
  override def *(other: Value): Value = Null
  override def /(other: Value): Value = Null
  override def >(other: Value): Boolean = false
  override def <(other: Value): Boolean = false
  override def >=(other: Value): Boolean = other == Null
  override def <=(other: Value): Boolean = other == Null
  override def contains(other: Value): Boolean = false
  override def unary_! : Value = Null
  override def unary_- : Value = Null
  override def apply(other: Value): Value = Null
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

  override def %(other: Value): Obj = {
    other match {
      case o: Obj ⇒
        this.%(o.v.toSeq)
      case Null ⇒
        Obj.empty
      case _ ⇒
        throw new UnsupportedOperationException(s"$this + $other")
    }
  }

  override def +(other: Value): Obj = {
    other match {
      case o: Obj ⇒
        this.+(o.v.toSeq)
      case Null ⇒
        this
      case _ ⇒
        throw new UnsupportedOperationException(s"$this + $other")
    }
  }

  override def -(other: Value): Obj = {
    other match {
      case o: Obj ⇒
        this.-(o.v.toSeq)
      case Null ⇒
        this
      case Lst(lst) ⇒ {
        val set = lst.toSet
        Obj(v.filterNot(el ⇒ set.contains(el._1)))
      }
      case _ ⇒
        Obj(v.filterNot(_._1 == other.toString))
    }
  }

  def % (other: Seq[(String,Value)]): Obj = {
    Obj(v ++ other.map {
      case (k, Null) ⇒ k → Null
      case (k, otherV) => k -> v.get(k).map {
        case originalV : Obj ⇒
          originalV.%(otherV)
        case _ ⇒
          otherV
      }.getOrElse {
        otherV
      }
    })
  }

  def + (other: Seq[(String,Value)]): Obj = {
    Obj(v ++ other.map {
      case (k, otherV) => k -> v.get(k).map { originalV ⇒
        originalV.+(otherV)
      }.getOrElse {
        otherV
      }
    })
  }

  def - (other: Seq[(String,Value)]): Obj = {
    Obj(v ++ other.flatMap {
      case (k, otherV) => v.get(k).flatMap { originalV ⇒
        (originalV, otherV) match {
          case (_: Obj, _: Obj | _: Lst) ⇒ Some(k -> originalV.-(otherV))
          case (_, Null) ⇒ Some(k -> originalV)
          case (_: Lst, _) ⇒ Some(k -> originalV.-(otherV))
          case (_: Obj, _: Bool | _: Number | _: Text) ⇒ Some(k → originalV.-(otherV))
          case _ ⇒ None
        }
      }
    } -- other.flatMap {
      case (k, otherV) => v.get(k).flatMap { originalV ⇒
        (originalV, otherV) match {
          case (_: Bool | _: Number | _: Text, _) ⇒ Some(k)
          case _ ⇒ None
        }
      }
    })
  }

  override def contains(other: Value): Boolean = v.contains(other.toString)

  override def apply(other: Value): Value = {
    Obj.extractValue(this, Obj.splitPath(other))
  }

  override def mkString(start: String, sep: String, end: String, alternative: Option[String]): String = {
    if (isEmpty && alternative.isDefined) {
      alternative.get
    }
    else {
      v.mkString(start, sep, end)
    }
  }
}

object Obj {
  val empty = new Obj(Map.empty)
  def apply(): Obj = empty

  // currently there is only mutable effective map that preserves order
  // so we use LinkedHashMap for that
  def from(v: (String,Value)*): Obj = {
    if (v.isEmpty)
      Obj.empty
    else
      new Obj(scala.collection.mutable.LinkedHashMap(v: _*))
  }

  def innerValue(path: Seq[String], value: Value): Obj = {
    if (path.tail.isEmpty) {
      Obj.from(path.head → value)
    }
    else {
      Obj.from(path.head → innerValue(path.tail, value))
    }
  }

  def extractValue(o: Obj, path: Seq[String]): Value = {
    if (path.tail.isEmpty) {
      o.v.get(path.head) match {
        case Some(v) ⇒ v
        case None ⇒ Null
      }
    }
    else {
      o.v.get(path.head) match {
        case Some(child: Obj) ⇒
          extractValue(child, path.tail)
        case _ ⇒
          Null
      }
    }
  }

  def splitPath(other: Value): Seq[String] = {
    other match {
      case Text(path) ⇒
        path.split('.')

      case Lst(elements) ⇒
        elements.map(_.toString)

      case _ ⇒
        throw new UnsupportedOperationException(s"Obj.splitPath($other)")
    }
  }

  def hasPath(o: Obj, path: Seq[String]): Boolean = {
    if (path.tail.isEmpty) {
      o.v.get(path.head) match {
        case Some(v) ⇒ true
        case None ⇒ false
      }
    }
    else {
      o.v.get(path.head) match {
        case Some(child: Obj) ⇒
          hasPath(child, path.tail)
        case _ ⇒
          false
      }
    }
  }
}

case class Lst(v: Seq[Value]) extends AnyVal with Value{
  override def ~~[T](visitor: ValueVisitor[T]): T = visitor.visitLst(this)

  override def +(other: Value): Lst = v :+ other
  override def -(other: Value): Lst = {
    v.diff(Seq(other))
  }

  override def ++(other: Value): Lst = {
    other match {
      case Lst(seqOther) ⇒ v ++ seqOther
      case Null ⇒ this
      case _ ⇒ throw new UnsupportedOperationException(s"$this ++ $other")
    }
  }

  override def --(other: Value): Lst = {
    other match {
      case Lst(seqOther) ⇒ v diff seqOther
      case Null ⇒ this
      case _ ⇒ throw new UnsupportedOperationException(s"$this ++ $other")
    }
  }

  override def contains(other: Value): Boolean = v.contains(other)

  override def apply(other: Value): Value = {
    val index = other.toInt
    v(index)
  }

  override def mkString(start: String, sep: String, end: String, alternative: Option[String]): String = {
    if (isEmpty && alternative.isDefined) {
      alternative.get
    }
    else {
      v.mkString(start, sep, end)
    }
  }
}

object Lst {
  val empty = Lst(Seq.empty)
  def apply(): Lst = empty

  def from(seq: Value*): Lst = {
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
    override def visitObj(d: Obj) = d.v.map(kv => kv._1 + "->" + kv._2).mkString("{", ",", "}")
    override def visitNumber(d: Number) = d.v.toString()
    override def visitLst(d: Lst) = d.v.mkString("[", ",", "]")
    override def visitNull(): String = "Null"
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

  val removeNullFieldsVisitor = new ValueVisitor[Value] {
    override def visitNumber(d: Number): Value = d
    override def visitNull(): Value = Null
    override def visitBool(d: Bool): Value = d
    override def visitObj(d: Obj): Value = Obj(d.v.flatMap {
      case (k, Null) ⇒ None
      case (k, other) ⇒ Some(k → removeNullFields(other))
    })
    override def visitText(d: Text): Value = d
    override def visitLst(d: Lst): Value = d
  }

  def removeNullFields(content: Value): Value = {
    content ~~ removeNullFieldsVisitor
  }

  def castUnavailable(s: String) = throw new ClassCastException(s)
}

case class ValueDynamicSelector(value: Value) extends AnyVal with Dynamic {
  def selectDynamic(name: String): Value = {
    value.toMap.getOrElse(
      if (name.startsWith("_") && name.length > 1) {
        name.substring(1)
      } else {
        name
      },
      Null
    )
  }
}
