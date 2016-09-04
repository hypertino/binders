import com.hypertino.inflector.naming.PlainConverter
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}

class TestBindOptionSpec extends FlatSpec with Matchers {
  "all Long, Option[Long], None parameters " should " bind" in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val i1 = 123456l
    val i2 = Some(555l)
    val i3: Option[Long] = None
    m.bind(i1)
    m.bind(i2)
    m.bind(i3)
    verify(m).writeLong(123456)
    verify(m).writeLong(555)
    verify(m).writeNull()
    verifyNoMoreInteractions(m)
  }

  "all Long parameters " should " unbind" in {
    val m = mock[TestDeserializer[PlainConverter.type]]
    when(m.readLong()).thenReturn(123456l)
    when(m.isNull).thenReturn(false)

    val i1 = m.unbind[Long]
    assert (i1 === 123456)
  }

  "all Option[Long] parameters " should " unbind" in {
    val m = mock[TestDeserializer[PlainConverter.type]]
    when(m.readLong()).thenReturn(555l)
    when(m.isNull).thenReturn(false)

    val i1 = m.unbind[Option[Long]]
    assert (i1 === Some(555))
  }

  "all None parameters " should " unbind" in {
    val m = mock[TestDeserializer[PlainConverter.type]]
    when(m.isNull).thenReturn(true)

    val i1 = m.unbind[Option[Long]]
    assert (i1 === None)
  }
}
