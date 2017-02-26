package com.hypertino.binders.value

import com.hypertino.binders.core.{BindOptions, Deserializer}
import com.hypertino.inflector.naming.Converter

import scala.language.experimental.macros

class ValueDeserializeException(message: String) extends RuntimeException(message)

abstract class ValueDeserializerBase[C <: Converter, I <: Deserializer[C]] (value: Value, val fieldName: Option[String])
  extends Deserializer[C] {

  protected def bindOptions: BindOptions

  def iterator(): Iterator[I] = {
    value match {
      case list: Lst => list.v
        .toIterator
        .map(createFieldDeserializer(_, None))
      case obj:Obj => obj.v
        .toIterator
        .map(kv => createFieldDeserializer(kv._2, Some(kv._1)))
      case Null ⇒ Iterator.empty
      case _ => throw new ValueDeserializeException("Couldn't iterate on: " + value)
    }
  }

  protected def createFieldDeserializer(value: Value, fieldName: Option[String]): I

  def isNull: Boolean = value == null || value == Null
  def readString(): String = value.toString
  def readInt(): Int = value.toInt
  def readLong(): Long = value.toLong
  def readDouble(): Double = value.toDouble
  def readFloat(): Float = value.toFloat
  def readBoolean(): Boolean = value.toBoolean
  def readBigDecimal(): BigDecimal = value.toBigDecimal
  def readValue(): Value = value
}

class ValueDeserializer[C <: Converter] (value: Value, override val fieldName: Option[String] = None)
                                        (implicit protected val bindOptions: BindOptions)
  extends ValueDeserializerBase[C, ValueDeserializer[C]](value, fieldName) {

  protected override def createFieldDeserializer(value: Value, fieldName: Option[String]): ValueDeserializer[C]
    = new ValueDeserializer[C](value, fieldName)
}
