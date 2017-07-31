import com.hypertino.binders.core.{BindOptions, Deserializer, Serializer}
import com.hypertino.binders.value.Value
import com.hypertino.inflector.naming.Converter

import scala.reflect.ClassTag

trait TestGeneric

trait TestDeserializer[C <: Converter] extends Deserializer[C] {
  def readInt(): Int

  def readIntNullable(): Option[Int]

  def readLong(): Long

  //def readLongNullable(): Option[Long]

  def readString(): String

  def readStringNullable(): Option[String]

  def isNull: Boolean

  def readValue(): Value

  def iterator(): Iterator[TestDeserializer[C]]

  def consume(): Unit
}

trait TestDeserializerWithGenerics[C <: Converter] extends Deserializer[C] {
  def readInt(): Int

  def readIntNullable(): Option[Int]

  def readLong(): Long

  def readString(): String

  def readStringNullable(): Option[String]

  def readMap[K, V](): Map[K, V]

  def isNull: Boolean

  def readValue(): Value

  def readGenericWihBounds[T <: TestGeneric](): T

  def iterator(): Iterator[TestDeserializerWithGenerics[C]]
}

trait TestDeserializerWithList[C <: Converter] extends Deserializer[C] {
  def readList[T: ClassTag](): List[T] = ???

  def iterator(): Iterator[TestDeserializer[C]]
}

trait TestSerializer[C <: Converter] extends Serializer[C] {
  def writeInt(value: Int)

  def writeIntNullable(value: Option[Int])

  def writeLong(value: Long)

  def writeString(value: String)

  def writeStringNullable(value: Option[String])

  def writeNull()

  def getFieldSerializer(fieldName: String): Option[TestSerializer[C]]

  def beginArray()

  def endArray()

  def beginObject()

  def endObject()
}

trait TestSerializerWithGenerics[C <: Converter] extends Serializer[C] {
  def writeInt(value: Int)

  def writeIntNullable(value: Option[Int])

  def writeSeq[T: ClassTag](value: Seq[T])

  def writeMap[K, V](value: Map[K, V])

  def writeMapNullable[K, V](value: Option[Map[K, V]])

  def writeLong(value: Long)

  def writeString(value: String)

  def writeStringNullable(value: Option[String])

  def writeGenericWithBounds[T <: TestGeneric](value: T)

  def writeNull()

  def getFieldSerializer(fieldName: String): Option[TestSerializer[C]]

  def beginArray()

  def endArray()
}
