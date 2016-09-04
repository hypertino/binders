import com.hypertino.inflector.naming.PlainConverter
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}
import org.mockito.Mockito._

case class TestClsWithInnerSet(data: Option[Map[String, Set[Int]]])

class TestCollectionsSpec extends FlatSpec with Matchers {

  def getMockList = {
    val testData = List(123456, 7890)
    val m = mock[TestDeserializerWithGenerics[PlainConverter.type]]
    val mi = testData.map {
      l =>
      {
        val mi = mock[TestDeserializerWithGenerics[PlainConverter.type]]
        when(mi.readInt()).thenReturn(l)
        mi
      }
    }
    when(m.iterator()).thenReturn(mi.toIterator)
    m
  }

  "Map[Long] " should " be bound" in {
    val m = mock[TestSerializerWithGenerics[PlainConverter.type]]
    val map: Map[Long,String] = Map(1l -> "a", 2l -> "b")
    m.bind(map)
    verify(m).writeMap(Map(1l -> "a", 2l -> "b"))
  }

  "CoolMap = Map[Long,String] " should " be bound" in {
    val m = mock[TestSerializerWithGenerics[PlainConverter.type]]
    val map: DefineType.CoolMap = Map(1l -> "a", 2l -> "b")
    m.bind(map)
    verify(m).writeMap(Map(1l -> "a", 2l -> "b"))
  }

  "Traversable of integers without explicit type " should " be bound" in {
    val m = mock[TestSerializerWithGenerics[PlainConverter.type]]
    val t:Traversable[Int] = Seq(123456, 7890).toTraversable
    m.bind(t)
    verify(m).beginArray()
    verify(m).writeInt(123456)
    verify(m).writeInt(7890)
    verify(m).endArray()
  }

  "Array of integers " should " be bound" in {
    val m = mock[TestSerializerWithGenerics[PlainConverter.type]]
    val t = Array(123456, 7890)
    m.bind(t)
    verify(m).beginArray()
    verify(m).writeInt(123456)
    verify(m).writeInt(7890)
    verify(m).endArray()
  }

  "Seq of integers without explicit type " should " be bound" in {
    val m = mock[TestSerializerWithGenerics[PlainConverter.type]]
    m.bind(Seq(123456, 7890))
    verify(m).writeSeq(Seq(123456, 7890))
  }

  "Vector of integers " should " unbind" in {
    val m = getMockList
    val l = m.unbind[Vector[Int]]
    assert (l === Vector(123456, 7890))
  }

  "List of integers " should " unbind" in {
    val m = getMockList
    val l = m.unbind[List[Int]]
    assert (l === List(123456, 7890))
  }

  "IndexedSeq of integers " should " unbind" in {
    val m = getMockList
    val l = m.unbind[IndexedSeq[Int]]
    assert (l === Vector(123456, 7890))
  }

  "Set of integers " should " unbind" in {
    val m = getMockList
    val l = m.unbind[Set[Int]]
    assert (l === Set(123456, 7890))
  }

  "Seq of integers " should " unbind" in {
    val m = getMockList
    val l = m.unbind[Seq[Int]]
    assert (l === Seq(123456, 7890))
  }

  "Iterator of integers " should " unbind" in {
    val m = getMockList
    val l: Iterator[Int] = m.unbind[Iterator[Int]]
    assert (l.toSeq === Seq(123456, 7890))
  }

  "List of integers " should " unbind directly" in {
    val m = mock[TestDeserializerWithList[PlainConverter.type]]
    when(m.readList[Int]()).thenReturn(List(123456, 7890))
    val l = m.unbind[List[Int]]
    assert (l === List(123456, 7890))
  }

  "Array of integers " should " unbind" in {
    val m = getMockList
    val l: Array[Int] = m.unbind[Array[Int]]
    assert (l.toSeq === Seq(123456, 7890))
  }

  /* todo: fix this:
  "1" should "2" in {

    //val m = getMockList// mock[TestDeserializer[PlainConverter.type]]
    val m = mock[TestDeserializer[PlainConverter.type]]
    m.unbind[TestCls]
  }
  */
}
