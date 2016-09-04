package com.hypertino.binders.value

import java.util.Date

import com.hypertino.binders.core.Deserializer
import com.hypertino.inflector.naming.Converter

import scala.language.experimental.macros

class ValueDeserializeException(message: String) extends RuntimeException(message)

class ValueDeserializerBase[C <: Converter, I <: Deserializer[C]] (value: Value, val fieldName: Option[String])
  extends Deserializer[C] {

  def iterator(): Iterator[I] = {
    value match {
      case s:Lst => s.v.toIterator.map(createFieldDeserializer(_, None))
      case m:Obj => m.v.toIterator.map(kv => createFieldDeserializer(kv._2, Some(kv._1)))
      case _ => throw new ValueDeserializeException("Couldn't iterate on: " + value)
    }
  }

  protected def createFieldDeserializer(value: Value, fieldName: Option[String]): I = ???

  def isNull: Boolean = value == null || value == Null
  def readString(): String = value.asString
  def readInt(): Int = value.asInt
  def readLong(): Long = value.asLong
  def readDouble(): Double = value.asDouble
  def readFloat(): Float = value.asFloat
  def readBoolean(): Boolean = value.asBoolean
  def readBigDecimal(): BigDecimal = value.asBigDecimal
  def readDate(): Date = value.asDate
  def readValue(): Value = value
}

class ValueDeserializer[C <: Converter] (value: Value, override val fieldName: Option[String] = None)
  extends ValueDeserializerBase[C, ValueDeserializer[C]](value, fieldName) {

  protected override def createFieldDeserializer(value: Value, fieldName: Option[String]): ValueDeserializer[C]
    = new ValueDeserializer[C](value, fieldName)
}
