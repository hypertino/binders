package com.hypertino.binders.value

import com.hypertino.binders.core.Deserializer
import com.hypertino.inflector.naming.{Converter, PlainConverter}

trait ValueSerializerFactory[C <: Converter, S <: ValueSerializerBase[C,_], D <: Deserializer[C]] {
  def withDeserializer[T](value: Value)(codeBlock: D ⇒ T): T = {
    val deserializer = createDeserializer(value)
    codeBlock(deserializer)
  }

  def withSerializer(codeBlock: S ⇒ Unit): Value = {
    val serializer = createSerializer()
    codeBlock(serializer)
    serializer.result
  }

  def createSerializer(): S
  def createDeserializer(value: Value): D
}

class DefaultValueSerializerFactory[C <: Converter] extends ValueSerializerFactory[C, ValueSerializer[C], ValueDeserializer[C]] {
  override def createSerializer(): ValueSerializer[C] = new ValueSerializer[C]()
  override def createDeserializer(value: Value): ValueDeserializer[C] = new ValueDeserializer[C](value)
}

object ValueSerializerFactory {
  implicit val defaultSerializerFactory = new DefaultValueSerializerFactory[PlainConverter.type]
  def findFactory[C <: Converter, S <: ValueSerializerBase[C,_], D <: Deserializer[C]]()
    (implicit factory: ValueSerializerFactory[C, S, D]): ValueSerializerFactory[C, S, D] = factory
}
