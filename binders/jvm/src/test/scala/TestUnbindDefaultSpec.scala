import com.hypertino.binders.core.BindersException
import com.hypertino.inflector.naming.PlainConverter
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}

case class TestDefault(intValue: Int = 123456,
                       intValue2: Int = 789,
                       stringValue: String = "abc")

class TestUnbindDefaultSpec extends FlatSpec with Matchers {
  val m = mock[TestDeserializer[PlainConverter.type]]

  val m1 = mock[TestDeserializer[PlainConverter.type]]
  when(m1.fieldName).thenReturn(Some("intValue"))
  when(m1.isNull).thenReturn(true)
  when(m1.readInt()).thenThrow(new BindersException("null value"))
  when(m1.readIntNullable()).thenReturn(None)

  val m2 = mock[TestDeserializer[PlainConverter.type]]
  when(m2.fieldName).thenReturn(Some("stringValue"))
  when(m2.isNull).thenReturn(true)
  when(m2.readString()).thenThrow(new BindersException("null value"))

  val mi = List(m1,m2)
  when(m.iterator()).thenReturn(mi.toIterator)

  val t = m.unbind[TestDefault]
  assert(t.intValue == 123456)
  assert(t.intValue2 == 789)
  assert(t.stringValue == "abc")
  assert(t === TestDefault(123456, 789, "abc"))
}
