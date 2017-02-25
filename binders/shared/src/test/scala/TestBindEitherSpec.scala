import com.hypertino.inflector.naming.PlainConverter
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

case class TestClass1(x:Int)

class TestBindEitherSpec extends FlatSpec with Matchers with MockFactory {
  "Either[Long,String] " should " bind" in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val i1: Either[Long,String] = Left(1)
    val i2: Either[Long,String] = Right("ha")
    val i3: Either[Long,Option[String]] = Right(None)

    inSequence {
      m.writeLong _ expects 1
      m.writeString _ expects "ha"
      m.writeStringNullable _ expects None
    }

    m.bind(i1)
    m.bind(i2)
    m.bind(i3)
  }

  "Either[Long,String] " should " unbind" in {
    import com.hypertino.binders.value._
    val m = mock[TestDeserializer[PlainConverter.type]]

    m.readValue _ expects() returning Number(123456l)
    val i1 = m.unbind[Either[Long,String]]
    i1 shouldBe Left(123456)

    m.readValue _ expects() returning Text("ha")
    val i2 = m.unbind[Either[Long,String]]
    i2 shouldBe Right("ha")

    m.readValue _ expects() returning Text("0")
    val i3 = m.unbind[Either[Long,Option[String]]]
    i3 shouldBe Right(Some("0"))

    m.readValue _ expects() returning Lst(Seq(1,2,3).map(_.toValue))
    val i4 = m.unbind[Either[TestClass1,Seq[Int]]]
    i4 shouldBe Right(Seq(1,2,3))
  }

  it should "prefer right for Either[_,_] if left & right conforms" in {
    import com.hypertino.binders.value._
    val m = mock[TestDeserializer[PlainConverter.type]]

    m.readValue _ expects() returning Number(123456l)
    val i4 = m.unbind[Either[Int,Long]]
    i4 shouldBe Right(123456l)
  }
}
