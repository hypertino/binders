import com.hypertino.binders.core.{BindOptions, Deserializer, Serializer}
import com.hypertino.binders.value.Value
import com.hypertino.inflector.naming.{Converter, PlainConverter}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

trait TestCustomDeserializer[C <: Converter] extends Deserializer[C] {
  def readCustom(): Custom
  def iterator(implicit bindOptions: BindOptions): Iterator[TestCustomDeserializer[C]]
}

trait TestCustomSerializer[C <: Converter] extends Serializer[C] {
  def writeCustomBase(value: CustomBase)
}

class TestBindCustomDeserializerClassSpec extends FlatSpec with Matchers with MockFactory {
  "Custom " should " bind" in {
    val m = mock[TestCustomSerializer[PlainConverter.type]]
    val custom = new Custom(123456)
    m.writeCustomBase _ expects custom
    m.bind(custom)
  }

  "Custom " should " unbind" in {
    val m = mock[TestCustomDeserializer[PlainConverter.type]]
    m.readCustom _ expects() returning new Custom(123456)
    val i1 = m.unbind[CustomBase]
    i1 shouldBe a[Custom]
  }
}
