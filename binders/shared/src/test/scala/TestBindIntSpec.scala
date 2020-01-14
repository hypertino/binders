import com.hypertino.inflector.naming.PlainConverter
import org.scalatest.{FlatSpec, Matchers}
import org.scalamock.scalatest.MockFactory

class TestBindIntSpec extends FlatSpec with Matchers with MockFactory {
  "all int parameters " should " bind" in {
    val m = mock[TestSerializer[PlainConverter.type]]

    inSequence {
      (m.writeInt _).expects(123456)
      (m.writeIntNullable _).expects(Some(555))
      (m.writeInt _).expects(7890)
    }

    val i1 = 123456
    val i2 = Some(555)
    val i3 = 7890
    m.bind(i1)
    m.bind(i2)
    m.bind(i3)
  }

  "all int parameters " should " bind as args" in {
    val m = mock[TestSerializer[PlainConverter.type]]
    inSequence {
      (m.writeInt _).expects(123456)
      (m.writeIntNullable _).expects(Some(555))
      (m.writeInt _).expects(7890)
    }
    val i1 = 123456
    val i2 = Some(555)
    val i3 = 7890
    m.bindArgs(i1, i2, i3)
  }

  "all int parameters " should " unbind" in {
    val m = mock[TestDeserializer[PlainConverter.type]]

    inSequence {
      (m.readInt _). expects () returning 123456
      (m.readIntNullable _). expects () returning Some(555)
    }

    val i1 = m.unbind[Int]
    val i2 = m.unbind[Option[Int]]
    i1 shouldBe 123456
    i2 shouldBe Some(555)
  }
}
