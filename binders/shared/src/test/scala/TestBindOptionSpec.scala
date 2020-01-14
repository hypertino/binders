import com.hypertino.inflector.naming.PlainConverter
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class TestBindOptionSpec extends FlatSpec with Matchers with MockFactory {
  "all Long, Option[Long], None parameters " should " bind" in {
    val m = mock[TestSerializer[PlainConverter.type]]
    inSequence {
      (m.writeLong _).expects(123456)
      (m.writeLong _).expects(555)
      (m.writeNull _).expects()
    }

    val i1 = 123456l
    val i2 = Some(555l)
    val i3: Option[Long] = None
    m.bind(i1)
    m.bind(i2)
    m.bind(i3)
  }

  "all Long parameters " should " unbind" in {
    val m = mock[TestDeserializer[PlainConverter.type]]
    inSequence {
      (m.readLong _). expects() returning 123456l
    }

    val i1 = m.unbind[Long]
    i1 shouldBe 123456
  }

  "all Option[Long] parameters " should " unbind" in {
    val m = mock[TestDeserializer[PlainConverter.type]]
    inSequence {
      (m.isNull _). expects () returning false
      (m.readLong _). expects() returning 555l
    }

    val i1 = m.unbind[Option[Long]]
    i1 shouldBe Some(555)
  }

  "all None parameters " should " unbind" in {
    val m = mock[TestDeserializer[PlainConverter.type]]

    (m.isNull _). expects () returning true

    val i1 = m.unbind[Option[Long]]
    i1 shouldBe None
  }
}
