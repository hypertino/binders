import com.hypertino.inflector.naming.PlainConverter
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}

class TestBindIntSpec extends FlatSpec with Matchers {
  "all int parameters " should " bind" in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val i1 = 123456
    val i2 = Some(555)
    val i3 = 7890
    m.bind(i1)
    m.bind(i2)
    m.bind(i3)
    verify(m).writeInt(123456)
    verify(m).writeIntNullable(Some(555))
    verify(m).writeInt(7890)
    verifyNoMoreInteractions(m)
  }

  "all int parameters " should " bind as args" in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val i1 = 123456
    val i2 = Some(555)
    val i3 = 7890
    m.bindArgs(i1, i2, i3)
    verify(m).writeInt(123456)
    verify(m).writeIntNullable(Some(555))
    verify(m).writeInt(7890)
    verifyNoMoreInteractions(m)
  }

  "all int parameters " should " unbind" in {
    val m = mock[TestDeserializer[PlainConverter.type]]
    when(m.readInt()).thenReturn(123456)
    when(m.readIntNullable()).thenReturn(Some(555))

    val i1 = m.unbind[Int]
    val i2 = m.unbind[Option[Int]]
    assert (i1 === 123456)
    assert (i2 === Some(555))
  }
}
