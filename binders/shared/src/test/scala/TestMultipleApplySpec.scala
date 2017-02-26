import com.hypertino.inflector.naming.PlainConverter
import org.scalatest.{FlatSpec, Matchers}
import org.scalamock.scalatest.MockFactory

case class MultipleApply(intValue: Int, stringValue: String)
object MultipleApply {
  def apply(intValue: Int): MultipleApply = MultipleApply(intValue, intValue.toHexString)
}

class TestMultipleApplySpec extends FlatSpec with Matchers with MockFactory {
  "MultipleApply " should " bind" in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val m1 = mock[TestSerializer[PlainConverter.type]]
    val m2 = mock[TestSerializer[PlainConverter.type]]

    inSequence {
      m.beginObject _ expects()
      m.getFieldSerializer _ expects "intValue" returning Some(m1)
      m1.writeInt _ expects 123456

      m.getFieldSerializer _ expects "stringValue" returning Some(m2)
      m2.writeString _ expects "abc"
      m.endObject _ expects()
    }

    m.bind(MultipleApply(123456, "abc"))
  }

  "MultipleApply " should " unbind" in {
    val m = mock[TestDeserializer[PlainConverter.type]]
    val m1 = mock[TestDeserializer[PlainConverter.type]]
    val m2 = mock[TestDeserializer[PlainConverter.type]]

    inSequence {
      m1.fieldName _ expects() returning Some("intValue")
      m1.readInt _ expects() returning 123456
    }

    inSequence {
      m2.fieldName _ expects() returning Some("stringValue")
      m2.readString _ expects() returning "abc"
    }

    inSequence{
      val mi = List(m1, m2)
      m.iterator _ expects () returning mi.toIterator
    }

    val t = m.unbind[MultipleApply]
    t shouldBe MultipleApply(123456, "abc")
  }
}
