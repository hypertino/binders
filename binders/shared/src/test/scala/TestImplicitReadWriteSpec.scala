import com.hypertino.binders.core.{Deserializer, Serializer}
import com.hypertino.inflector.naming.{Converter, PlainConverter}
import org.scalatest.{FlatSpec, Matchers}
import org.scalamock.scalatest.MockFactory

case class SomeData(x: Int)
trait SomeDataBuilder[T] {
  def makeT: T
}
object SomeData {
  implicit val builder = new SomeDataBuilder[SomeData]{
    def makeT = null
  }
}

trait TestDeserializerWithImplicit[C <: Converter] extends Deserializer[C] {
  def readSome[T <: SomeDataBuilder[SomeData]](implicit builder: T): SomeData
  def iterator(): Iterator[TestDeserializerWithImplicit[C]]
}


trait TestSerializerWithImplicit[C <: Converter] extends Serializer[C] {
  def writeSome[T <: SomeDataBuilder[SomeData]](value: SomeData)(implicit builder: T)

  def writeNull()

  def getFieldSerializer(fieldName: String): Option[TestSerializerWithImplicit[C]]

  def beginArray()

  def endArray()

  def beginObject()

  def endObject()
}


case class TestClassWithData(someData: SomeData)

class TestImplicitReadWriteSpec extends FlatSpec with Matchers with MockFactory {
  "fields " should " be deserialized by reader with implicit parameter" in {
    val m = mock[TestDeserializerWithImplicit[PlainConverter.type]]
    val m1 = mock[TestDeserializerWithImplicit[PlainConverter.type]]

    m1.fieldName _ expects () returning Some("someData")
    (m1.readSome( _: SomeDataBuilder[SomeData])).expects (SomeData.builder) returning SomeData(123)

    val mi = List(m1)
    m.iterator _ expects () returning mi.toIterator

    val t = m.unbind[TestClassWithData]
    t shouldBe TestClassWithData(SomeData(123))
  }

  "fields " should " be serialized by writer with implicit parameter" in {
    val m = mock[TestSerializerWithImplicit[PlainConverter.type]]
    val m1 = mock[TestSerializerWithImplicit[PlainConverter.type]]

    inSequence {
      m.beginObject _ expects()
      m.getFieldSerializer _ expects "someData" returning Some(m1)
      (m1.writeSome(_: SomeData)( _: SomeDataBuilder[SomeData])).expects (SomeData(123), SomeData.builder)
      m.endObject _ expects()
    }

    m.bind(TestClassWithData(SomeData(123)))
  }
}
