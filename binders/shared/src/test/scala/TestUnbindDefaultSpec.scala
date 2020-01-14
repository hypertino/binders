import com.hypertino.inflector.naming.PlainConverter
import org.scalatest.{FlatSpec, Matchers}
import org.scalamock.scalatest.MockFactory

case class TestDefault(
                        intValue: Int = 123456,
                        intValue2: Int = 789,
                        stringValue: String = "abc",
                        longValue: Long = 555l
                      )

class TestUnbindDefaultSpec extends FlatSpec with Matchers with MockFactory {
  val m1 = mock[TestDeserializer[PlainConverter.type]]
  val m2 = mock[TestDeserializer[PlainConverter.type]]
  val m3 = mock[TestDeserializer[PlainConverter.type]]
  val m4 = mock[TestDeserializer[PlainConverter.type]]
  val m5 = mock[TestDeserializer[PlainConverter.type]]

  inSequence {
    (m2.fieldName _). expects(). returning (Some("intValue"))
    (m2.readIntNullable _). expects(). returning (None)
    (m3.fieldName _). expects(). returning (Some("nonExistingField"))
    (m3.consume _). expects(). returning (())
    (m4.fieldName _). expects(). returning (Some("stringValue"))
    (m4.readStringNullable _). expects(). returning (None)
    (m5.fieldName _). expects(). returning (Some("longValue"))
    (m5.isNull _). expects(). returning (true)
  }
  inSequence{
    val mi = List(m2, m3, m4, m5)
    (m1.iterator _). expects (). returning (mi.toIterator)
  }

  val t = m1.unbind[TestDefault]
  t.intValue shouldBe 123456
  t.intValue2 shouldBe 789
  t.stringValue shouldBe "abc"
  t.longValue shouldBe 555l
  t shouldBe TestDefault(123456, 789, "abc", 555l)
}
