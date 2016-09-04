import com.hypertino.binders.core.{ImplicitDeserializer, ImplicitSerializer}
import com.hypertino.inflector.naming.PlainConverter
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}

class Custom(initValue: Int) {
  override def toString() = initValue.toString
  def intValue = initValue
}

class CustomSerializer extends ImplicitSerializer[Custom, TestSerializer[_]] {
  override def write(serializer: TestSerializer[_], value: Custom) = serializer.writeInt(value.intValue)
}

class CustomDeserializer extends ImplicitDeserializer[Custom, TestDeserializer[_]] {
  override def read(deserializer: TestDeserializer[_]): Custom = new Custom(deserializer.readInt())
}

class TestBindCustomClassSpec extends FlatSpec with Matchers {
  "Custom " should " bind" in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val i1 = new Custom(123456)
    implicit val customSerializer = new CustomSerializer
    m.bind(i1)
    verify(m).writeInt(123456)
    verifyNoMoreInteractions(m)
  }

  "Custom " should " bind as args" in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val i1 = new Custom(123456)
    implicit val customSerializer = new CustomSerializer
    m.bindArgs(i1)
    verify(m).writeInt(123456)
    verifyNoMoreInteractions(m)
  }

  "Custom " should " unbind" in {
    val m = mock[TestDeserializer[PlainConverter.type]]
    when(m.readInt()).thenReturn(123456)
    implicit val customDeserializer = new CustomDeserializer
    val i1 = m.unbind[Custom]
    assert (i1.intValue === 123456)
  }
}
