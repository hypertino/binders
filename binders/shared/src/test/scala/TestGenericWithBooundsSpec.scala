import com.hypertino.inflector.naming.PlainConverter
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class TestGenericSuper(x: String) extends TestGeneric

class TestGenericWithBooundsSpec extends FlatSpec with Matchers with MockFactory {
  "Generic with bounds " should " bind refined type" in {
    val m = mock[TestSerializerWithGenerics[PlainConverter.type]]
    val g = new TestGeneric {}
    m.writeGenericWithBounds _ expects g
    m.bind(g)
  }

  "Generic with bounds " should " bind" in {
    val m = mock[TestSerializerWithGenerics[PlainConverter.type]]
    val g = new TestGenericSuper("1")
    m.writeGenericWithBounds _ expects g
    m.bind(g)
  }

  "Generic with bounds " should " unbind" in {
    val m = mock[TestDeserializerWithGenerics[PlainConverter.type]]
    val r = new TestGeneric{}
    m.readGenericWihBounds[TestGeneric] _ expects () returning r
    val g = m.unbind[TestGeneric]
    g shouldBe r
  }
}
