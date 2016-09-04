import com.hypertino.inflector.naming.PlainConverter
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}


case class MultipleApply(intValue: Int, stringValue: String)
object MultipleApply {
  def apply(intValue: Int): MultipleApply = MultipleApply(intValue, intValue.toHexString)
}


class TestMultipleApplySpec extends FlatSpec with Matchers {
  "MultipleApply " should " bind" in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val m1 = mock[TestSerializer[PlainConverter.type]]
    val m2 = mock[TestSerializer[PlainConverter.type]]

    when(m.getFieldSerializer("intValue")).thenReturn(Some(m1))
    when(m.getFieldSerializer("stringValue")).thenReturn(Some(m2))

    m.bind(MultipleApply(123456, "abc"))

    verify(m).beginObject()
    verify(m).getFieldSerializer("intValue")
    verify(m1).writeInt(123456)
    verifyNoMoreInteractions(m1)

    verify(m).getFieldSerializer("stringValue")
    verify(m2).writeString("abc")
    verifyNoMoreInteractions(m2)
    verify(m).endObject()
    verifyNoMoreInteractions(m)
  }

  "MultipleApply " should " unbind" in {
    val m = mock[TestDeserializer[PlainConverter.type]]

    val m1 = mock[TestDeserializer[PlainConverter.type]]
    when(m1.fieldName).thenReturn(Some("intValue"))
    when(m1.readInt()).thenReturn(123456)

    val m2 = mock[TestDeserializer[PlainConverter.type]]
    when(m2.fieldName).thenReturn(Some("stringValue"))
    when(m2.readString()).thenReturn("abc")

    val mi = List(m1,m2)
    when(m.iterator()).thenReturn(mi.toIterator)

    val t = m.unbind[MultipleApply]
    assert(t === MultipleApply(123456, "abc"))
  }
}
