import com.hypertino.binders.core.BindOptions
import com.hypertino.inflector.naming.PlainConverter
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

case class TestStringCollections(seq: Seq[String], map: Map[String, String])
case class TestStringListCls(list: List[String])
case class TestStringVectorCls(list: Vector[String])
case class TestStringIndexedSeqCls(list: IndexedSeq[String])
case class TestStringSetCls(list: Set[String])
case class TestArrayCls(array: Array[String])

class TestCollectionsInClassSpec extends FlatSpec with Matchers with MockFactory {

  "all case class collection fields " should " be serialized by names " in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val m1 = mock[TestSerializer[PlainConverter.type]]
    val m2 = mock[TestSerializer[PlainConverter.type]]
    val m3 = mock[TestSerializer[PlainConverter.type]]

    inSequence {
      m.beginObject _ expects ()
      m.getFieldSerializer _ expects "seq" returning Some(m1)
      m1.beginArray _ expects ()
      m1.writeString _ expects "a"
      m1.writeString _ expects "b"
      m1.endArray _ expects ()
      m.getFieldSerializer _ expects "map" returning Some(m2)
      m2.beginObject _ expects ()
      m2.getFieldSerializer _ expects "x" returning Some(m3)
      m3.writeString _ expects "y"
      m2.endObject _ expects ()
      m.endObject _ expects ()
    }

    val col = TestStringCollections(Seq("a","b"), Map("x" → "y"))
    m.bind(col)
  }

  "case class array field " should " be serialized by name " in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val m1 = mock[TestSerializer[PlainConverter.type]]

    inSequence {
      m.beginObject _ expects ()
      m.getFieldSerializer _ expects "array" returning Some(m1)
      m1.beginArray _ expects ()
      m1.writeString _ expects "100500"
      m1.writeString _ expects "abyrvalg"
      m1.endArray _ expects ()
      m.endObject _ expects ()
    }

    val col = TestArrayCls(Array("100500", "abyrvalg"))
    m.bind(col)
  }

  "empty class collection fields " should " NOT be skipped according to option" in {
    val m = mock[TestSerializer[PlainConverter.type]]
    val m1 = mock[TestSerializer[PlainConverter.type]]
    val m2 = mock[TestSerializer[PlainConverter.type]]

    inSequence {
      m.beginObject _ expects ()
      m.getFieldSerializer _ expects "seq" returning Some(m1)
      m1.beginArray _ expects ()
      m1.endArray _ expects ()
      m.getFieldSerializer _ expects "map" returning Some(m2)
      m2.beginObject _ expects ()
      m2.endObject _ expects ()
      m.endObject _ expects ()
    }

    m.bind(TestStringCollections(Seq.empty, Map.empty))
  }

  "empty class collection fields " should " be skipped according to option" in {
    val m = mock[TestSerializer[PlainConverter.type]]

    inSequence {
      m.beginObject _ expects ()
      m.endObject _ expects ()
    }

    implicit val bo: BindOptions = new BindOptions(true)
    m.bind(TestStringCollections(Seq.empty, Map.empty))
  }

  "all case class collections " should " be deserialized by names " in {
    val m = mock[TestDeserializer[PlainConverter.type]]
    val m1 = mock[TestDeserializer[PlainConverter.type]]

    inSequence {
      m1.fieldName _ expects() returning Some("seq")
      val miIter = List("a","b").toIterator map { s ⇒
        val me = mock[TestDeserializer[PlainConverter.type]]
        me.readString _ expects () returning s
        me
      }

      m1.iterator _ expects () returning miIter
    }

    inSequence {
      val mci = List(m1)
      m.iterator _ expects () returning mci.toIterator
    }

    val t = m.unbind[TestStringCollections]
    t shouldBe TestStringCollections(Seq("a","b"), Map.empty)
  }

  "empty List in case class " should " be deserialized by names " in {
    val m = mock[TestDeserializer[PlainConverter.type]]
    m.iterator _ expects () returning Iterator.empty

    val t = m.unbind[TestStringListCls]
    t shouldBe TestStringListCls(List.empty)
  }

  "empty Vector in case class " should " be deserialized by names " in {
    val m = mock[TestDeserializer[PlainConverter.type]]
    m.iterator _ expects () returning Iterator.empty

    val t = m.unbind[TestStringVectorCls]
    t shouldBe TestStringVectorCls(Vector.empty)
  }

  "empty IndexedSeq in case class " should " be deserialized by names " in {
    val m = mock[TestDeserializer[PlainConverter.type]]
    m.iterator _ expects () returning Iterator.empty

    val t = m.unbind[TestStringIndexedSeqCls]
    t shouldBe TestStringIndexedSeqCls(IndexedSeq.empty)
  }

  "empty Set in case class " should " be deserialized by names " in {
    val m = mock[TestDeserializer[PlainConverter.type]]
    m.iterator _ expects () returning Iterator.empty

    val t = m.unbind[TestStringSetCls]
    t shouldBe TestStringSetCls(Set.empty)
  }

  "empty Array in case class " should " be deserialized by names " in {
    val m = mock[TestDeserializer[PlainConverter.type]]
    m.iterator _ expects () returning Iterator.empty

    val t = m.unbind[TestArrayCls]
    t.array.length shouldBe 0
  }
}
