import com.hypertino.binders.annotations.fieldName
import com.hypertino.binders.core.{BindOptions, Deserializer}
import com.hypertino.inflector.naming.{CamelCaseToSnakeCaseConverter, PlainConverter}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

object DefineType {
  type CoolMap = Map[Long,String]
  type StringMap = Map[String,String]
}

case class TestProduct(intValue1: Int, nullableValue: Option[Int], intValue2: Int)
case class TestInnerProduct(inner: TestProduct, nullableInner: Option[TestProduct], nullableInner1: Option[TestProduct])
case class TestProductAnnotated(@fieldName("f1Value") intValue1Ant: Int, @fieldName("f2Value", true) intValue2: Int)

trait TestTrait {
  def intValue: Int
  def stringValue: String
}

object TestTrait {
  def apply(intValue: Int, stringValue: String): TestTrait = TraitContainer(intValue, stringValue)
  def unapply(t: TestTrait) = Some((t.intValue, t.stringValue))
}

trait TestTraitAnnotated {
  def intValue: Int
  def stringValue: String
}

object TestTraitAnnotated {
  def apply(@fieldName("f1Value") intValue: Int, @fieldName("f2Value", true) stringValue: String): TestTraitAnnotated = TraitContainer(intValue, stringValue)
  def unapply(t: TestTraitAnnotated) = Some((t.intValue, t.stringValue))
}

case class TraitContainer(intValue: Int, stringValue: String) extends TestTrait with TestTraitAnnotated

trait TestTrait2 {
  def intValue: Int
  def stringValue: String
}

case class TraitContainer2(intValue: Int, stringValue: String) extends TestTrait2

case class TestRecursiveCaseClass(stringValue: String, recursive: Seq[TestRecursiveCaseClass])

class TestProductSpec extends FlatSpec with Matchers with MockFactory {
  "all case class fields " should " be serialized by names " in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val m1 = mock[TestSerializer[PlainConverter.type]]
    val m2 = mock[TestSerializer[PlainConverter.type]]
    val m3 = mock[TestSerializer[PlainConverter.type]]

    inSequence {
      m.beginObject _ expects()
      m.getFieldSerializer _ expects "intValue1" returning Some(m1)
      m1.writeInt _ expects 123456

      m.getFieldSerializer _ expects "nullableValue" returning Some(m2)
      m2.writeIntNullable _ expects Some(555)

      m.getFieldSerializer _ expects "intValue2" returning Some(m3)
      m3.writeInt _ expects 7890
      m.endObject _ expects()
    }

    m.bind(TestProduct(123456, Some(555), 7890))
  }

  "all case class fields " should " be serialized by names with specified convention" in {
    val m = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m1 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m2 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m3 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]

    inSequence {
      m.beginObject _ expects()
      m.getFieldSerializer _ expects "int_value1" returning Some(m1)
      m1.writeInt _ expects 123456
      m.getFieldSerializer _ expects "nullable_value" returning Some(m2)
      m2.writeIntNullable _ expects Some(555)
      m.getFieldSerializer _ expects "int_value2" returning Some(m3)
      m3.writeInt _ expects 7890
      m.endObject _ expects()
    }

    m.bind(TestProduct(123456, Some(555), 7890))
  }

  "some case class fields " should " be serialized by names " in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val m1 = mock[TestSerializer[PlainConverter.type]]

    inSequence {
      m.beginObject _ expects()
      m.getFieldSerializer _ expects "intValue1" returning Some(m1)
      m1.writeInt _ expects 123456

      m.getFieldSerializer _ expects "nullableValue" returning None
      m.getFieldSerializer _ expects "intValue2" returning None
      m.endObject _ expects()
    }

    m.bindPartial(TestProduct(123456, Some(555), 7890))
  }

  "only! some case class fields " should " be serialized by names (skipOptionalFields=true)" in {
    val m = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m1 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m2 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m3 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]

    inSequence {
      m.beginObject _ expects()
      m.getFieldSerializer _ expects "int_value1" returning Some(m1)
      m1.writeInt _ expects 888666777
      m.getFieldSerializer _ expects "int_value2" returning Some(m3)
      m3.writeInt _ expects 7890
      m.endObject _ expects ()
    }

    //m.getFieldSerializer _ expects "nullable_value" returning None

    implicit val op3: BindOptions = new BindOptions(true)
    m.bind(TestProduct(888666777, None, 7890))
  }

  "By default all null fields " should " be serialized by names (skipOptionalFields=false)" in {
    val m = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m1 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m2 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m3 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]

    inSequence {
      m.beginObject _ expects()
      m.getFieldSerializer _ expects "int_value1" returning Some(m1)
      m1.writeInt _ expects 888666777
      m.getFieldSerializer _ expects "nullable_value" returning Some(m2)
      m2.writeIntNullable _ expects None
      m.getFieldSerializer _ expects "int_value2" returning Some(m3)
      m3.writeInt _ expects 7890
      m.endObject _ expects()
    }

    m.bind(TestProduct(888666777, None, 7890))
  }

  "all case class fields " should " be deserialized by names " in {
    val m = mock[TestDeserializer[PlainConverter.type]]
    val m1 = mock[TestDeserializer[PlainConverter.type]]
    val m2 = mock[TestDeserializer[PlainConverter.type]]
    val m3 = mock[TestDeserializer[PlainConverter.type]]


    m1.fieldName _ expects () returning Some("intValue1")
    m1.readInt _ expects () returning 123456

    m2.fieldName _ expects () returning Some("nullableValue")
    m2.readIntNullable _ expects () returning Some(555)

    m3.fieldName _ expects () returning Some("intValue2")
    m3.readInt _ expects () returning 7890

    val mi = List(m1,m2,m3)
    m.iterator _ expects () returning mi.toIterator

    val t = m.unbind[TestProduct]
    t shouldBe TestProduct(123456, Some(555), 7890)
  }

  "all case class fields " should " be deserialized by names with specified convention" in {
    val m = mock[TestDeserializer[CamelCaseToSnakeCaseConverter.type]]

    val m1 = mock[TestDeserializer[CamelCaseToSnakeCaseConverter.type]]
    m1.fieldName _ expects () returning Some("int_value1")
    m1.readInt _ expects () returning 123456

    val m2 = mock[TestDeserializer[CamelCaseToSnakeCaseConverter.type]]
    m2.fieldName _ expects () returning Some("nullable_value")
    m2.readIntNullable _ expects () returning Some(555)

    val m3 = mock[TestDeserializer[CamelCaseToSnakeCaseConverter.type]]
    m3.fieldName _ expects () returning Some("int_value2")
    m3.readInt _ expects () returning 7890

    val mi = List(m1,m2,m3)
    m.iterator _ expects () returning mi.toIterator

    val t = m.unbind[TestProduct]
    t shouldBe TestProduct(123456, Some(555), 7890)
  }

  "some case class fields " should " be deserialized by names " in {
    val m = mock[TestDeserializer[PlainConverter.type]]

    val m1 = mock[TestDeserializer[PlainConverter.type]]
    m1.fieldName _ expects () returning Some("intValue1")
    m1.readInt _ expects () returning 123456

    val m3 = mock[TestDeserializer[PlainConverter.type]]
    m3.fieldName _ expects () returning Some("intValue2")
    m3.readInt _ expects () returning 7890

    val mi = List(m1,m3)
    m.iterator _ expects () returning mi.toIterator

    val t = m.unbindPartial(TestProduct(-1, Some(555), -2))
    t shouldBe TestProduct(123456, Some(555), 7890)
  }

  "all inner case class fields " should " be deserialized by names " in {
    val m1 = mock[TestDeserializer[PlainConverter.type]]
    m1.fieldName _ expects () returning Some("intValue1") twice()
    m1.readInt _ expects () returning 123456 twice()

    val m2 = mock[TestDeserializer[PlainConverter.type]]
    m2.fieldName _ expects () returning Some("nullableValue") twice()
    m2.readIntNullable _ expects () returning Some(555) twice()

    val m3 = mock[TestDeserializer[PlainConverter.type]]
    m3.fieldName _ expects () returning Some("intValue2") twice()
    m3.readInt _ expects () returning 7890 twice()

    val mi = List(m1,m2,m3)

    val mf1 = mock[TestDeserializer[PlainConverter.type]]
    mf1.fieldName _ expects () returning Some("inner")
    mf1.iterator _ expects () returning mi.toIterator

    val mn = mock[TestDeserializer[PlainConverter.type]]
    mn.fieldName _ expects () returning Some("nullableInner")
    mn.isNull _ expects () returning true

    val mf2 = mock[TestDeserializer[PlainConverter.type]]
    mf2.fieldName _ expects () returning Some("nullableInner1")
    mf2.isNull _ expects () returning false
    mf2.iterator _ expects () returning mi.toIterator

    val mo = mock[TestDeserializer[PlainConverter.type]]
    val moi = List(mf1,mn,mf2)
    mo.iterator _ expects () returning moi.toIterator

    val t = mo.unbind[TestInnerProduct]
    t shouldBe TestInnerProduct(
      TestProduct(123456, Some(555), 7890),
      None,
      Some(TestProduct(123456, Some(555), 7890))
    )
  }

  "annotated fields of case class " should " be serialized " in {
    val m = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m1 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m2 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]

    inSequence {
      m.beginObject _ expects()
      m.getFieldSerializer _ expects "f1Value" returning Some(m1)
      m1.writeInt _ expects 576
      m.getFieldSerializer _ expects "f2_value" returning Some(m2)
      m2.writeInt _ expects 90
      m.endObject _ expects ()
    }
    m.bind(TestProductAnnotated(576, 90))
  }

  "annotated fields of trait with companion " should " be serialized " in {
    val m = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m1 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m2 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]

    inSequence {
      m.beginObject _ expects()
      m.getFieldSerializer _ expects "f1Value" returning Some(m1)
      m1.writeInt _ expects 576
      m.getFieldSerializer _ expects "f2_value" returning Some(m2)
      m2.writeString _ expects "90"
      m.endObject _ expects ()
    }
    m.bind(TestTraitAnnotated(576, "90"))
  }

  "trait fields " should " be serialized" in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val m1 = mock[TestSerializer[PlainConverter.type]]
    val m2 = mock[TestSerializer[PlainConverter.type]]

    inSequence {
      m.beginObject _ expects()
      m.getFieldSerializer _ expects "intValue" returning Some(m1)
      m1.writeInt _ expects 123456

      m.getFieldSerializer _ expects "stringValue" returning Some (m2)
      m2.writeString _ expects "abc"

      m.endObject _ expects()
    }

    m.bind(TestTrait(123456, "abc"))
  }

  "trait fields " should " be deserialized" in {
    val m = mock[TestDeserializer[PlainConverter.type]]

    val m1 = mock[TestDeserializer[PlainConverter.type]]
    m1.fieldName _ expects () returning Some("intValue")
    m1.readInt _ expects () returning 123456

    val m2 = mock[TestDeserializer[PlainConverter.type]]
    m2.fieldName _ expects () returning Some("stringValue")
    m2.readString _ expects () returning "abc"

    val mi = List(m1,m2)
    m.iterator _ expects () returning mi.toIterator

    val t = m.unbind[TestTrait]
    t.intValue shouldBe 123456
    t.stringValue shouldBe "abc"
    t shouldBe TestTrait(123456, "abc")
  }

  "partial annotated fields of trait with companion " should " be serialized " in {
    val m = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m1 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]
    val m2 = mock[TestSerializer[CamelCaseToSnakeCaseConverter.type]]

    inSequence {
      m.beginObject _ expects()
      m.getFieldSerializer _ expects "f1Value" returning Some(m1)
      m1.writeInt _ expects 576
      m.getFieldSerializer _ expects "f2_value" returning Some(m2)
      m2.writeString _ expects "90"
      m.endObject _ expects ()
    }
    m.bindPartial(TestTraitAnnotated(576, "90"))
  }

  "partial trait fields " should " be serialized" in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val m1 = mock[TestSerializer[PlainConverter.type]]
    val m2 = mock[TestSerializer[PlainConverter.type]]

    inSequence {
      m.beginObject _ expects()
      m.getFieldSerializer _ expects "intValue" returning Some(m1)
      m1.writeInt _ expects 123456

      m.getFieldSerializer _ expects "stringValue" returning Some (m2)
      m2.writeString _ expects "abc"

      m.endObject _ expects()
    }

    m.bindPartial(TestTrait(123456, "abc"))
  }

  "more than 22 case class fields " should " be serialized " in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val m1 = mock[TestSerializer[PlainConverter.type]]
    inSequence {
      m.beginObject _ expects()
      for (i <- 1 to 23) {
        m.getFieldSerializer _ expects s"a$i" returning Some(m1)
        m1.writeInt _ expects i
      }
      m.endObject _ expects()
    }
    m.bind(MoreThan22Fields(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23))
  }

  "more than 22 case class fields " should " be deserialized " in {
    val m = mock[TestDeserializer[PlainConverter.type]]
    if (false){
      m.unbind[MoreThan22Fields]
    }
  }


  "recursive case class " should " be serialized " in {
    val m = mock[TestSerializer[PlainConverter.type]]
    if (false) {
      m.bind(TestRecursiveCaseClass("Hey", Seq(TestRecursiveCaseClass("Yey", Seq.empty))))
    }
  }

  "recursive case class " should " be deserialized " in {
    val m = mock[TestDeserializer[PlainConverter.type]]
    if (false){
      m.unbind[TestRecursiveCaseClass]
    }
  }

  //  this doesn't work yet
//  "partial trait fields " should " be serialized even if there is no companion object" in {
//    val m = mock[TestSerializer[PlainConverter.type]]
//    val m1 = mock[TestSerializer[PlainConverter.type]]
//    val m2 = mock[TestSerializer[PlainConverter.type]]
//
//    inSequence {
//      m.beginObject _ expects()
//      m.getFieldSerializer _ expects "intValue" returning Some(m1)
//      m1.writeInt _ expects 123456
//
//      m.getFieldSerializer _ expects "stringValue" returning Some (m2)
//      m2.writeString _ expects "abc"
//
//      m.endObject _ expects()
//    }
//
//    val t: TestTrait2 = TraitContainer2(123456, "abc")
//    m.bindPartial(t)
//  }
  // todo: inner class serialization
}
