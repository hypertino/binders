import com.hypertino.binders.annotations.fieldName
import com.hypertino.binders.core.BindOptions
import com.hypertino.inflector.naming.{CamelCaseToSnakeCaseConverter, PlainConverter}
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}

object DefineType {
  type CoolMap = Map[Long,String]
  type StringMap = Map[String,String]
}

case class TestProduct(intValue1: Int, nullableValue: Option[Int], intValue2: Int)
case class TestInnerProduct(inner: TestProduct, nullableInner: Option[TestProduct], nullableInner1: Option[TestProduct])
case class TestProductAnnotated(@fieldName("f1Value") intValue1: Int, @fieldName("f2Value", true) intValue2: Int)

trait TestTrait {
  def intValue: Int
  def stringValue: String
}

object TestTrait {
  def apply(intValue: Int, stringValue: String): TestTrait = TraitContainer(intValue, stringValue)
  def unapply(t: TestTrait) = Some((t.intValue, t.stringValue))
}

case class TraitContainer(intValue: Int, stringValue: String) extends TestTrait

class TestProductSpec extends FlatSpec with Matchers {
  "all case class fields " should " be serialized by names " in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val m1 = mock[TestSerializer[PlainConverter.type]]
    val m2 = mock[TestSerializer[PlainConverter.type]]
    val m3 = mock[TestSerializer[PlainConverter.type]]

    when(m.getFieldSerializer("intValue1")).thenReturn(Some(m1))
    when(m.getFieldSerializer("nullableValue")).thenReturn(Some(m2))
    when(m.getFieldSerializer("intValue2")).thenReturn(Some(m3))

    m.bind(TestProduct(123456, Some(555), 7890))

    verify(m).beginObject()
    verify(m).getFieldSerializer("intValue1")
    verify(m1).writeInt(123456)
    verifyNoMoreInteractions(m1)

    verify(m).getFieldSerializer("nullableValue")
    verify(m2).writeIntNullable(Some(555))
    verifyNoMoreInteractions(m2)

    verify(m).getFieldSerializer("intValue2")
    verify(m3).writeInt(7890)
    verifyNoMoreInteractions(m3)
    verify(m).endObject()

    verifyNoMoreInteractions(m)
  }

  "all case class fields " should " be serialized by names with specified convention" in {
    val m = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m1 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m2 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m3 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]

    when(m.getFieldSerializer("int_value1")).thenReturn(Some(m1))
    when(m.getFieldSerializer("nullable_value")).thenReturn(Some(m2))
    when(m.getFieldSerializer("int_value2")).thenReturn(Some(m3))

    m.bind(TestProduct(123456, Some(555), 7890))

    verify(m).beginObject()
    verify(m).getFieldSerializer("int_value1")
    verify(m1).writeInt(123456)
    verifyNoMoreInteractions(m1)

    verify(m).getFieldSerializer("nullable_value")
    verify(m2).writeIntNullable(Some(555))
    verifyNoMoreInteractions(m2)

    verify(m).getFieldSerializer("int_value2")
    verify(m3).writeInt(7890)
    verifyNoMoreInteractions(m3)
    verify(m).endObject()

    verifyNoMoreInteractions(m)
  }

  "some case class fields " should " be serialized by names " in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val m1 = mock[TestSerializer[PlainConverter.type]]

    when(m.getFieldSerializer("intValue1")).thenReturn(Some(m1))
    when(m.getFieldSerializer("nullableValue")).thenReturn(None)
    when(m.getFieldSerializer("intValue2")).thenReturn(None)

    m.bindPartial(TestProduct(123456, Some(555), 7890))

    verify(m).beginObject()
    verify(m).getFieldSerializer("intValue1")
    verify(m1).writeInt(123456)
    verifyNoMoreInteractions(m1)

    verify(m).getFieldSerializer("nullableValue")
    verify(m).getFieldSerializer("intValue2")
    verify(m).endObject()

    verifyNoMoreInteractions(m)
  }

  "only! some case class fields " should " be serialized by names (skipOptionalFields=true)" in {

    val m = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m1 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m2 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m3 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]

    when(m.getFieldSerializer("int_value1")).thenReturn(Some(m1))
    when(m.getFieldSerializer("nullable_value")).thenReturn(Some(m2))
    when(m.getFieldSerializer("int_value2")).thenReturn(Some(m3))

    implicit val op3: BindOptions = new BindOptions(true)
    m.bind(TestProduct(888666777, None, 7890))

    verify(m).beginObject()
    verify(m).getFieldSerializer("int_value1")
    verify(m1).writeInt(888666777)
    verifyNoMoreInteractions(m1)

    //verify(m).getFieldSerializer("nullable_value")
    //verify(m2).writeIntNullable(Some(555))
    verifyNoMoreInteractions(m2)

    verify(m).getFieldSerializer("int_value2")
    verify(m3).writeInt(7890)
    verifyNoMoreInteractions(m3)
    verify(m).endObject()

    verifyNoMoreInteractions(m)
  }

  "By default all null fields " should " be serialized by names (skipOptionalFields=false)" in {
    val m = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m1 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m2 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m3 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]

    when(m.getFieldSerializer("int_value1")).thenReturn(Some(m1))
    when(m.getFieldSerializer("nullable_value")).thenReturn(Some(m2))
    when(m.getFieldSerializer("int_value2")).thenReturn(Some(m3))

    m.bind(TestProduct(888666777, None, 7890))

    verify(m).beginObject()
    verify(m).getFieldSerializer("int_value1")
    verify(m1).writeInt(888666777)
    verifyNoMoreInteractions(m1)

    verify(m).getFieldSerializer("nullable_value")
    verify(m2).writeIntNullable(None)
    verifyNoMoreInteractions(m2)

    verify(m).getFieldSerializer("int_value2")
    verify(m3).writeInt(7890)
    verifyNoMoreInteractions(m3)
    verify(m).endObject()

    verifyNoMoreInteractions(m)
  }

  "all case class fields " should " be deserialized by names " in {
    val m = mock[TestDeserializer[PlainConverter.type]]

    val m1 = mock[TestDeserializer[PlainConverter.type]]
    when(m1.fieldName).thenReturn(Some("intValue1"))
    when(m1.readInt()).thenReturn(123456)

    val m2 = mock[TestDeserializer[PlainConverter.type]]
    when(m2.fieldName).thenReturn(Some("nullableValue"))
    when(m2.readIntNullable()).thenReturn(Some(555))

    val m3 = mock[TestDeserializer[PlainConverter.type]]
    when(m3.fieldName).thenReturn(Some("intValue2"))
    when(m3.readInt()).thenReturn(7890)

    val mi = List(m1,m2,m3)
    when(m.iterator()).thenReturn(mi.toIterator)

    val t = m.unbind[TestProduct]
    assert(t === TestProduct(123456, Some(555), 7890))
  }

  "all case class fields " should " be deserialized by names with specified convention" in {
    val m = mock[TestDeserializer[CamelCaseToSnakeCaseConverter.type]]

    val m1 = mock[TestDeserializer[CamelCaseToSnakeCaseConverter.type]]
    when(m1.fieldName).thenReturn(Some("int_value1"))
    when(m1.readInt()).thenReturn(123456)

    val m2 = mock[TestDeserializer[CamelCaseToSnakeCaseConverter.type]]
    when(m2.fieldName).thenReturn(Some("nullable_value"))
    when(m2.readIntNullable()).thenReturn(Some(555))

    val m3 = mock[TestDeserializer[CamelCaseToSnakeCaseConverter.type]]
    when(m3.fieldName).thenReturn(Some("int_value2"))
    when(m3.readInt()).thenReturn(7890)

    val mi = List(m1,m2,m3)
    when(m.iterator()).thenReturn(mi.toIterator)

    val t = m.unbind[TestProduct]
    assert(t === TestProduct(123456, Some(555), 7890))
  }

  "some case class fields " should " be deserialized by names " in {
    val m = mock[TestDeserializer[PlainConverter.type]]

    val m1 = mock[TestDeserializer[PlainConverter.type]]
    when(m1.fieldName).thenReturn(Some("intValue1"))
    when(m1.readInt()).thenReturn(123456)

    val m3 = mock[TestDeserializer[PlainConverter.type]]
    when(m3.fieldName).thenReturn(Some("intValue2"))
    when(m3.readInt()).thenReturn(7890)

    val mi = List(m1,m3)
    when(m.iterator()).thenReturn(mi.toIterator)

    val t = m.unbindPartial(TestProduct(-1, Some(555), -2))
    assert(t === TestProduct(123456, Some(555), 7890))
  }

  "all inner case class fields " should " be deserialized by names " in {

    val m1 = mock[TestDeserializer[PlainConverter.type]]
    when(m1.fieldName).thenReturn(Some("intValue1"))
    when(m1.readInt()).thenReturn(123456)

    val m2 = mock[TestDeserializer[PlainConverter.type]]
    when(m2.fieldName).thenReturn(Some("nullableValue"))
    when(m2.readIntNullable()).thenReturn(Some(555))

    val m3 = mock[TestDeserializer[PlainConverter.type]]
    when(m3.fieldName).thenReturn(Some("intValue2"))
    when(m3.readInt()).thenReturn(7890)

    val mi = List(m1,m2,m3)

    val mf1 = mock[TestDeserializer[PlainConverter.type]]
    when(mf1.fieldName).thenReturn(Some("inner"))
    when(mf1.iterator()).thenReturn(mi.toIterator)

    val mn = mock[TestDeserializer[PlainConverter.type]]
    when(mn.isNull).thenReturn(true)
    when(mn.fieldName).thenReturn(Some("nullableInner"))

    val mf2 = mock[TestDeserializer[PlainConverter.type]]
    when(mf2.fieldName).thenReturn(Some("nullableInner1"))
    when(mf2.iterator()).thenReturn(mi.toIterator)

    val mo = mock[TestDeserializer[PlainConverter.type]]
    val moi = List(mf1,mn,mf2)
    when(mo.iterator()).thenReturn(moi.toIterator)

    val t = mo.unbind[TestInnerProduct]
    assert(t === TestInnerProduct(
      TestProduct(123456, Some(555), 7890),
      None,
      Some(TestProduct(123456, Some(555), 7890))
    ))
  }

  "annotated field " should " be serialized " in {

    val m = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m1 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m2 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]

    when(m.getFieldSerializer("f1Value")).thenReturn(Some(m1))
    when(m.getFieldSerializer("f2_value")).thenReturn(Some(m2))
    when(m.getFieldSerializer("intValue1")).thenThrow(new RuntimeException("Ew"))
    when(m.getFieldSerializer("intValue2")).thenThrow(new RuntimeException("Ew"))

    m.bind(TestProductAnnotated(576, 90))

    verify(m).beginObject()
    verify(m).getFieldSerializer("f1Value")
    verify(m1).writeInt(576)
    verifyNoMoreInteractions(m1)
    verify(m).getFieldSerializer("f2_value")
    verify(m2).writeInt(90)
    verifyNoMoreInteractions(m2)
    verify(m).endObject()
    verifyNoMoreInteractions(m)
  }

  "trait fields " should " be serialized" in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val m1 = mock[TestSerializer[PlainConverter.type]]
    val m2 = mock[TestSerializer[PlainConverter.type]]

    when(m.getFieldSerializer("intValue")).thenReturn(Some(m1))
    when(m.getFieldSerializer("stringValue")).thenReturn(Some(m2))

    m.bind(TestTrait(123456, "abc"))

    verify(m).beginObject()
    verify(m).getFieldSerializer("intValue")
    verify(m1).writeInt(123456)
    verifyNoMoreInteractions(m1)

    verify(m).getFieldSerializer("stringValue")
    verify(m2).writeString("abc")
    verifyNoMoreInteractions(m2)
    verify(m).endObject()

    verifyNoMoreInteractions(m)
  }

  "trait fields " should " be deserialized" in {
    val m = mock[TestDeserializer[PlainConverter.type]]

    val m1 = mock[TestDeserializer[PlainConverter.type]]
    when(m1.fieldName).thenReturn(Some("intValue"))
    when(m1.readInt()).thenReturn(123456)

    val m2 = mock[TestDeserializer[PlainConverter.type]]
    when(m2.fieldName).thenReturn(Some("stringValue"))
    when(m2.readString()).thenReturn("abc")

    val mi = List(m1,m2)
    when(m.iterator()).thenReturn(mi.toIterator)

    val t = m.unbind[TestTrait]
    assert(t.intValue == 123456)
    assert(t.stringValue == "abc")
    assert(t === TestTrait(123456, "abc"))
  }
  // todo: inner class serialization
}
