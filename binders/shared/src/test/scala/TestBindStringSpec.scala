import com.hypertino.inflector.naming.PlainConverter
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class TestBindStringSpec extends FlatSpec with Matchers with MockFactory {
  "all string parameters " should " bind" in {
    val m = mock[TestSerializer[PlainConverter.type]]

    inSequence {
      m.writeString _ expects "123456"
      m.writeStringNullable _ expects Some("555")
      m.writeString _ expects "7890"
    }

    val s1 = "123456"
    val s2 = Some("555")
    val s3 = "7890"
    m.bind(s1)
    m.bind(s2)
    m.bind(s3)
  }

  "all string parameters " should " bind as args" in {
    val m = mock[TestSerializer[PlainConverter.type]]
    inSequence {
      m.writeString _ expects "123456"
      m.writeStringNullable _ expects Some("555")
      m.writeString _ expects "7890"
    }
    val s1 = "123456"
    val s2 = Some("555")
    val s3 = "7890"
    m.bindArgs(s1, s2, s3)
  }

  "all string parameters " should " unbind" in {
    val m = mock[TestDeserializer[PlainConverter.type]]

    inSequence {
      m.readString _ expects () returning "123456"
      m.readStringNullable _ expects () returning Some("555")
    }

    val s1 = m.unbind[String]
    val s2 = m.unbind[Option[String]]
    s1 shouldBe "123456"
    s2 shouldBe Some("555")
  }
}
