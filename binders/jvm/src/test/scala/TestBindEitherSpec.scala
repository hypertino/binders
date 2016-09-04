import com.hypertino.binders.value.{Lst, Number, Text}
import com.hypertino.inflector.naming.PlainConverter
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}

case class TestClass1(x:Int)

class TestBindEitherSpec extends FlatSpec with Matchers {
  "Either[Long,String] " should " bind" in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val i1: Either[Long,String] = Left(1)
    val i2: Either[Long,String] = Right("ha")
    val i3: Either[Long,Option[String]] = Right(None)
    m.bind(i1)
    m.bind(i2)
    m.bind(i3)
    verify(m).writeLong(1)
    verify(m).writeString("ha")
    verify(m).writeNull()
    verifyNoMoreInteractions(m)
  }

  "Either[Long,String] " should " unbind" in {
    import com.hypertino.binders.value._
    val m = mock[TestDeserializer[PlainConverter.type]]
    when(m.readDynamic()).thenReturn(Number(123456l))
    val i1 = m.unbind[Either[Long,String]]

    when(m.readDynamic()).thenReturn(Text("ha"))
    val i2 = m.unbind[Either[Long,String]]

    when(m.readDynamic()).thenReturn(Text("0"))
    val i3 = m.unbind[Either[Long,Option[String]]]

    when(m.readDynamic()).thenReturn(Lst(Seq(1,2,3).map(_.toValue)))
    val i4 = m.unbind[Either[TestClass1,Seq[Int]]]

    assert (i1 === Left(123456))
    assert (i2 === Right("ha"))
    assert (i3 === Right(Some("0")))
    assert (i4 === Right(Seq(1,2,3)))
  }
}
