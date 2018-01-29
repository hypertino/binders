package com.hypertino.binders.value

import java.util.Date

import com.hypertino.binders.core.{BindOptions, Serializer}
import com.hypertino.inflector.naming.Converter

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.experimental.macros

class ValueSerializeException(message: String) extends RuntimeException(message)

trait ValueSerializerBaseTrait[C <: Converter] extends Serializer[C] {
  def result: Value
}

abstract class ValueSerializerBase[C <: Converter, F <: ValueSerializerBaseTrait[C]] extends ValueSerializerBaseTrait[C]{
  protected var value: Value = _
  protected var map: scala.collection.mutable.Map[String, ValueSerializerBaseTrait[C]] = _
  protected var seq: scala.collection.mutable.ArrayBuffer[Value] = _
  protected var stack: mutable.ArrayBuffer[scala.collection.mutable.ArrayBuffer[Value]] = null //new ArrayBuffer[ArrayBuffer[Value]]()

  protected def bindOptions: BindOptions

  def getFieldSerializer(fieldName: String): Option[F] = {
    if (map == null) {
      throw new ValueSerializeException("Can't get field serializer for non-map field: "+ fieldName)
    }

    val f = createFieldSerializer()
    map += fieldName -> f
    Some(f)
  }

  protected def createFieldSerializer(): F

  def writeNull() = writeValue(Null)

  def writeString(value: String) = if(value == null) writeNull() else writeValue(Text(value))
  def writeBoolean(value: Boolean) = writeValue(Bool(value))
  def writeBigDecimal(value: BigDecimal) = if(value == null) writeNull() else writeValue(Number(value))
  def writeInt(value: Int) = writeValue(Number(value))
  def writeLong(value: Long) = writeValue(Number(value))
  def writeFloat(value: Float) = writeValue(Number(BigDecimal(value.toDouble)))
  def writeDouble(value: Double) = writeValue(Number(value))
  def writeDate(value: Date) = if(value == null) writeNull() else writeValue(Number(value.getTime))

  def writeValue(value: Value): Unit = {
    if (seq != null)
      seq += value
    else
      this.value = value
  }

  def beginObject(): Unit = {
    if (seq != null) {
      pushSeq()
    }
    map = new scala.collection.mutable.HashMap[String, ValueSerializerBaseTrait[C]]()
  }

  def endObject(): Unit = {
    val o = Obj(map.toMap.map(kv => (kv._1, kv._2.result)))
    if (isStackEmpty) {
      value = o
      map = null
    }
    else {
      popSeq()
      seq += o
    }
  }

  def beginArray(): Unit = {
    if (seq != null) {
      pushSeq()
    }
    seq = new scala.collection.mutable.ArrayBuffer[Value]()
  }

  def endArray(): Unit = {
    val s = Lst(seq)
    if (isStackEmpty) {
      value = s
      seq = null
    }
    else {
      popSeq()
      seq += s
    }
  }

  protected def isStackEmpty: Boolean = stack == null || stack.isEmpty

  protected def pushSeq(): Unit = {
    if (isStackEmpty) {
      stack = new ArrayBuffer[ArrayBuffer[Value]]()
    }
    stack += seq
  }

  protected def popSeq(): Unit = {
    if (isStackEmpty) {
      throw new IllegalStateException("Stack is empty, can't deserialize")
    }
    seq = stack.remove(stack.size -1)
  }

  def result: Value = value
}

class ValueSerializer[C <: Converter] (implicit protected val bindOptions: BindOptions)
  extends ValueSerializerBase[C, ValueSerializer[C]]{
  protected override def createFieldSerializer(): ValueSerializer[C] = new ValueSerializer[C]()
}
