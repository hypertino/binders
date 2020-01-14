import com.hypertino.binders.core.{ImplicitDeserializer, ImplicitSerializer}
import com.hypertino.inflector.naming.PlainConverter
import org.scalatest.{FlatSpec, Matchers}
import org.scalamock.scalatest.MockFactory

trait CustomBase

class Custom(initValue: Int) extends CustomBase {
  override def toString = initValue.toString
  def intValue = initValue
}

class ImplicitCustomSerializer extends ImplicitSerializer[Custom, TestSerializer[_]] {
  override def write(serializer: TestSerializer[_], value: Custom) = serializer.writeInt(value.intValue)
}

class ImplicitCustomDeserializer extends ImplicitDeserializer[Custom, TestDeserializer[_]] {
  override def read(deserializer: TestDeserializer[_]): Custom = new Custom(deserializer.readInt())
}

class TestBindImplicitCustomClassSpec extends FlatSpec with Matchers with MockFactory {
  "Custom " should " bind" in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val i1 = new Custom(123456)
    implicit val customSerializer = new ImplicitCustomSerializer

    (m.writeInt _). expects (123456)
    m.bind(i1)
  }

  "Custom " should " bind as args" in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val i1 = new Custom(123456)
    implicit val customSerializer = new ImplicitCustomSerializer
    (m.writeInt _). expects (123456)
    m.bindArgs(i1)
  }

  "Custom " should " unbind" in {
    val m = mock[TestDeserializer[PlainConverter.type]]
    implicit val customDeserializer = new ImplicitCustomDeserializer
    (m.readInt _). expects() returning 123456
    val i1 = m.unbind[Custom]
    i1.intValue shouldBe 123456
  }
}
