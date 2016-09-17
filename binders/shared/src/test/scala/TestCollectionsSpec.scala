import com.hypertino.inflector.naming.PlainConverter
import org.scalatest.{FlatSpec, Matchers}
import org.scalamock.scalatest.MockFactory

case class TestClsWithInnerSet(data: Option[Map[String, Set[Int]]])

class TestCollectionsSpec extends FlatSpec with Matchers with MockFactory {

  def getMockList = {
    val testData = List(123456, 7890)
    val m = mock[TestDeserializerWithGenerics[PlainConverter.type]]
    val mi = testData.map {
      l =>
      {
        val mi = mock[TestDeserializerWithGenerics[PlainConverter.type]]
        mi.readInt _ expects() returning l
        mi
      }
    }
    m.iterator _ expects() returning mi.toIterator
    m
  }

  "Map[Long] " should " be bound" in {
    val m = mock[TestSerializerWithGenerics[PlainConverter.type]]
    val map: Map[Long,String] = Map(1l -> "a", 2l -> "b")

    m.writeMap[Long,String] _ expects Map(1l -> "a", 2l -> "b")
    m.bind(map)
  }

  "CoolMap = Map[Long,String] " should " be bound" in {
    val m = mock[TestSerializerWithGenerics[PlainConverter.type]]
    val map: DefineType2.CoolMap = Map(1l -> "a", 2l -> "b")
    m.writeMap[Long,String] _ expects Map(1l -> "a", 2l -> "b")
    m.bind(map)
  }

  "Traversable of integers without explicit type " should " be bound" in {
    val m = mock[TestSerializerWithGenerics[PlainConverter.type]]
    val t:Traversable[Int] = Seq(123456, 7890).toTraversable
    inSequence {
      m.beginArray _ expects ()
      m.writeInt _ expects 123456
      m.writeInt _ expects 7890
      m.endArray _ expects ()
    }
    m.bind(t)
  }

  "Array of integers " should " be bound" in {
    val m = mock[TestSerializerWithGenerics[PlainConverter.type]]
    val t = Array(123456, 7890)
    inSequence {
      m.beginArray _ expects ()
      m.writeInt _ expects 123456
      m.writeInt _ expects 7890
      m.endArray _ expects ()
    }
    m.bind(t)
  }

  ignore should "Seq of integers without explicit type be bound" in {
    val m = mock[TestSerializerWithGenerics[PlainConverter.type]]
    m.writeSeq _ expects Seq(123456, 7890)
    m.bind(Seq(123456, 7890))
  }

  "Vector of integers " should " unbind" in {
    val m = getMockList
    val l = m.unbind[Vector[Int]]
    l shouldBe Vector(123456, 7890)
  }

  "List of integers " should " unbind" in {
    val m = getMockList
    val l = m.unbind[List[Int]]
    l shouldBe List(123456, 7890)
  }

  "IndexedSeq of integers " should " unbind" in {
    val m = getMockList
    val l = m.unbind[IndexedSeq[Int]]
    l shouldBe Vector(123456, 7890)
  }

  "Set of integers " should " unbind" in {
    val m = getMockList
    val l = m.unbind[Set[Int]]
    assert (l === Set(123456, 7890))
  }

  "Seq of integers " should " unbind" in {
    val m = getMockList
    val l = m.unbind[Seq[Int]]
    l shouldBe Seq(123456, 7890)
  }

  "Iterator of integers " should " unbind" in {
    val m = getMockList
    val l: Iterator[Int] = m.unbind[Iterator[Int]]
    l.toSeq shouldBe Seq(123456, 7890)
  }

  // todo: fix ScalaMock support this: def readList[T: ClassTag](): List[T] = ???
  ignore should "List of integers unbind directly" in {
    val m = mock[TestDeserializerWithList[PlainConverter.type]]
    m.readList[Int] _ expects() returning List(123456, 7890)
    val l = m.unbind[List[Int]]
    l shouldBe List(123456, 7890)
  }

  "Array of integers " should " unbind" in {
    val m = getMockList
    val l: Array[Int] = m.unbind[Array[Int]]
    l.toSeq shouldBe Seq(123456, 7890)
  }
}
