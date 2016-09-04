import java.util._

import com.hypertino.inflector.naming.PlainConverter
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}
import org.mockito.Mockito._

class TestDateSpec extends FlatSpec with Matchers {
  val (yesterday, now) = {
    val cal = Calendar.getInstance()
    cal.setTime(new Date())
    cal.add(Calendar.DATE, -11)
    (cal.getTime(), new Date())
  }

  "all java.util.Date parameters " should " bind" in {
    val m = mock[TestSerializer[PlainConverter.type]]
    m.bind(yesterday)
    m.bind(Some(yesterday))
    m.bind(now)
    verify(m).writeDate(yesterday)
    verify(m).writeDateNullable(Some(yesterday))
    verify(m).writeDate(now)
    verifyNoMoreInteractions(m)
  }

  "java.util.Date " should " unbind" in {
    val m = mock[TestDeserializer[PlainConverter.type]]
    when (m.readDate()).thenReturn(yesterday)
    when (m.readDateNullable()).thenReturn(Some(now))
    val d1 = m.unbind[Date]
    val d2 = m.unbind[Option[Date]]
    assert (d1 === yesterday)
    assert (d2 === Some(now))
  }
}
