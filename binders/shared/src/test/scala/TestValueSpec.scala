import com.hypertino.binders.value._
import org.scalatest._

case class TestValue(a:Int, b:String, c:Boolean)

object DefineType2 {
  type CoolMap = Map[Long,String]
  type StringMap = Map[String,String]
}

class TestValueSpec extends FlatSpec with Matchers {

  "toValue " should " serialize int " in {
    val i1 = 123456
    val d1 = i1.toValue
    assert (d1.toInt == 123456)
  }

  "to[Int] " should " deserialize int " in {
    val d1 = Number(123456)
    val i1 = d1.to[Int]
    assert (i1 == 123456)
  }

  "toValue " should " serialize long " in {
    val i1 = 123456l
    val d1 = i1.toValue
    assert (d1.toLong == 123456l)
  }

  "to[Long] " should " deserialize long " in {
    val d1 = Number(Long.MaxValue)
    val i1 = d1.to[Long]
    assert (i1 == Long.MaxValue)
  }

  "toValue " should " serialize string " in {
    val i1 = "yey"
    val d1 = i1.toValue
    assert (d1.toString == "yey")
  }

  "to[String] " should " deserialize string " in {
    val d1 = Text("ho")
    val i1 = d1.to[String]
    assert (i1 == "ho")
  }

  "toValue " should " serialize null " in {
    val i1: String = null
    val d1 = i1.toValue
    assert (d1 == Null)
  }

  "to[Option[String]] " should " deserialize null " in {
    val d1 = Null
    val i1 = d1.to[Option[String]]
    assert (i1.isEmpty)
  }

  "toValue " should " serialize Seq[Int] " in {
    val i1 = Seq(1,2,3)
    val d1 = i1.toValue
    d1.toSeq should equal (Seq(1,2,3).map(Number(_)))
  }

  "to[Seq[Int]] " should " deserialize Seq[Int] " in {
    val d1 = Lst(Seq(Number(1),Text("2"),Number(3)))
    val i1 = d1.to[Seq[Int]]
    i1 should equal (Seq(1,2,3))
  }

  "to[Seq[Value]] " should " deserialize Seq[Value] " in {
    val d1 = Lst(Seq(Number(1),Text("2"),Number(3)))
    val i1 = d1.to[Seq[Value]]
    i1 shouldBe Seq[Value](1,"2",3)
  }

  "toValue " should " serialize Map[String,Int] " in {
    val i1: Map[String,Int] = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val d1 = i1.toValue
    d1.toMap should equal (i1 map toValueNumberPair)
  }

  "to[Map[String,Int]] " should " deserialize Map[String,Int] " in {
    val m = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val d1 = Obj(m map toValueNumberPair)
    val i1 = d1.to[Map[String,Int]]
    i1 should equal (m)
  }

  "toValue " should " serialize Obj " in {
    val i1 = TestValue(1,"ho",true)
    val d1 = i1.toValue
    d1.toMap should equal (Map("a" -> Number(1), "b" -> Text("ho"), "c" -> Bool(true)))
  }

  "valueAs " should " deserialize StringMap = Map[String,String] " in {
    import DefineType2._

    val m = Obj.from("a" -> "he", "b" -> "ho")
    val map = m.to[StringMap]
    map should equal(Map("a"->"he", "b"->"ho"))
  }

  "Value " should " allow selectDynamic " in {
    val d = Obj.from("a" -> 1, "b" -> "ho", "c" -> true, "_" -> false,
      "inner" → Obj.from("x" → "100500")
    )
    val a = d.dynamic.a
    a should equal(Number(1))

    val b = d.dynamic.b
    b should equal(Text("ho"))

    val f = d.dynamic.__
    f should equal(Bool(false))

    //val i = d.inner.x // todo: this doesn't work under scala 2.10, uncomment in the future
    //i shouldBe Text("100500")
  }

  "Obj " should " merge (+) " in {
    val value1 = Obj.from("a" -> 1, "b" -> "ho", "d" → 5, "e" → "kl")
    val value2 = Obj.from("a" -> 2, "b" -> "no", "d" → Null)
    val value3 = value1 + value2
    value3 should equal(Obj.from("a" -> 3, "b" -> "hono", "d" → Null, "e" → "kl"))
  }

  "Obj " should " merge (+) inner fields" in {
    val value1 = Obj.from("a" -> Obj.from("x" → 1, "y" → "yey"))
    val value2 = Obj.from("a" -> Obj.from("z" → "hey"))
    val value3 = value1 + value2
    value3 shouldBe Obj.from("a" -> Obj.from("x" → 1, "y" → "yey", "z" → "hey"))
  }

  "Obj " should " subtract (-) " in {
    val value1 = Obj.from("a" -> 1, "b" -> "ho", "c" -> true, "d" → 5, "e" → "kl")
    val value2 = Obj.from("a" -> 1, "d" → 8)
    val value3 = value1 - value2
    value3 should equal(Obj.from("b" -> "ho", "c" -> true, "e" → Text("kl")))
  }


  "Obj " should " subtract (-) inner fields" in {
    val value1 = Obj.from("a" -> Obj.from("x" → 1, "z" → "hey", "y" → "yey"))
    val value2 = Obj.from("a" -> Obj.from("z" → "hey"))
    val value3 = value1 - value2
    value3 shouldBe Obj.from("a" -> Obj.from("x" → 1, "y" → "yey"))
  }

  "Obj " should " preserve order of fields " in {
    val seq = Seq[(String, Value)]("a" -> 1, "b" -> "ho", "c" -> true, "d" → 5, "e" → "kl")
    val value1 = Obj.from(seq: _*)

    val z = seq.zipWithIndex.map(a ⇒ a._2 → a._1).toMap

    value1.v.zipWithIndex.foreach{
      case (kv, index) ⇒
        z(index) should equal(kv)
    }
  }

  "Obj " should " return inner fields" in {
    val value1 = Obj.from("a" -> 1, "b" -> "ho", "c" -> true, "d" → 5, "e" → Obj.from("x" → 100500))

    value1("a") should equal(Number(1))
    value1("e.x") should equal(Number(100500))
    value1(Lst.from("e","x")) should equal(Number(100500))
    value1("e.x.z") should equal(Null)
    value1(Lst.from("e","x","z")) should equal(Null)

    Obj.hasPath(value1, Obj.splitPath("e.x")) shouldBe true
    Obj.hasPath(value1, Obj.splitPath("e.y")) shouldBe false
  }

  "implicits" should "do conversion" in {
    val obj = Obj.from("a" → 5, "b" → "18")
    obj should equal(Obj(Map("a"→Number(5), "b"→Text("18"))))

    val obj2 = Obj.from("a" → "b")
    obj2 should equal(Obj(Map("a"→Text("b"))))

    val lst = Lst(Seq("a",1,false))
    lst should equal(Lst(Seq(Text("a"),Number(1),Bool(false))))
  }

  "Value " should "do pattern matching" in {
    val obj = Obj.from("a" → 5, "b" → "18")
    obj match {
      case Obj(map) ⇒ // fine
    }

    val lst: Value = Lst.from("a",1,false)
    lst match {
      case Lst(seq) ⇒ // fine
      case Bool(_) ⇒ fail
      case Null ⇒ fail
      case Number(_) ⇒ fail
      case Obj(_) ⇒ fail
      case Text(_) ⇒ fail
    }
  }

  "Number operators " should "work" in {
    Number(2) + Number(3) shouldBe Number(5)
    Number(3) - Number(1) shouldBe Number(2)
    Number(3) * Number(2) shouldBe Number(6)
    Number(6) / Number(2) shouldBe Number(3)
    Number(5) % Number(4) shouldBe Number(1)
    Number(5) > Number(4) shouldBe true
    Number(5) < Number(4) shouldBe false
    Number(4) > Number(5) shouldBe false
    Number(4) < Number(5) shouldBe true
    Number(5) > Number(5) shouldBe false
    Number(5) < Number(5) shouldBe false
    Number(5) >= Number(4) shouldBe true
    Number(5) <= Number(4) shouldBe false
    Number(4) >= Number(5) shouldBe false
    Number(4) <= Number(5) shouldBe true
    Number(5) >= Number(5) shouldBe true
    Number(5) <= Number(5) shouldBe true

    Number(1) | Number(2) shouldBe Number(1|2)
    Number(5) & Number(6) shouldBe Number(5&6)
    Number(5) ^ Number(6) shouldBe Number(5^6)

    !Number(5) shouldBe Number(~5)
    -Number(5) shouldBe Number(-5)
  }

  "Text operators " should "work" in {
    Text("a") + Text("b") shouldBe Text("ab")
    Text("a") + Number(10) shouldBe Text("a10")
    Text("b") > Text("a") shouldBe true
    Text("b") < Text("a") shouldBe false
    Text("a") > Text("b") shouldBe false
    Text("a") < Text("b") shouldBe true
    Text("b") > Text("b") shouldBe false
    Text("b") < Text("b") shouldBe false
    Text("b") >= Text("a") shouldBe true
    Text("b") <= Text("a") shouldBe false
    Text("a") >= Text("b") shouldBe false
    Text("a") <= Text("b") shouldBe true
    Text("b") >= Text("b") shouldBe true
    Text("b") <= Text("b") shouldBe true
    Text("abc").contains("bc") shouldBe true
    Text("abc").contains("xbc") shouldBe false
  }

  "Null operators " should "work" in {
    Null + Number(3) shouldBe Null
    Null - Number(1) shouldBe Null
    Null * Number(2) shouldBe Null
    Null / Number(2) shouldBe Null
    Null % Number(4) shouldBe Null
    Null > Number(4) shouldBe false
    Null < Number(4) shouldBe false
    Null >= Number(4) shouldBe false
    Null <= Number(4) shouldBe false
    Null >= Null shouldBe true
    Null <= Null shouldBe true
    !Null shouldBe Null
    -Null shouldBe Null
  }

  "Lst operators " should "work" in {
    Lst.from(1,2,3) ++ Lst.from(4,5) shouldBe Lst.from(1,2,3,4,5)
    Lst.from(1,2,3) ++ Null shouldBe Lst.from(1,2,3)
    Null ++ Lst.from(1,2,3) shouldBe Lst.from(1,2,3)
    Null -- Lst.from(1,2,3) shouldBe Null
    Lst.from(1,2,3) + 4 shouldBe Lst.from(1,2,3,4)
    Lst.from(1,2,3) -- Lst.from(2) shouldBe Lst.from(1,3)
    Lst.from(1,2,3) - 2 shouldBe Lst.from(1,3)
    Lst.from(1,2,3).contains(2) shouldBe true
    Lst.from(1,2,3).contains(4) shouldBe false
  }

  "Lst " should " return by index" in {
    val value1 = Lst.from(12,22,32)

    value1("1") should equal(Number(22))
    value1(2) should equal(Number(32))
  }

  "Obj operators " should "work" in {
    Obj.from("a" → 1) + Obj.from("b" → 2) shouldBe Obj.from("a" → 1, "b" → 2)
    Null + Obj.from("a" → 1) shouldBe Obj.from("a" → 1)
    Obj.from("a" → 1, "b" → Obj.from("x" → 2)) + Obj.from("b" → Null) shouldBe Obj.from("a" → 1, "b" → Obj.from("x" → 2))
    Obj.from("b" → Null) + Obj.from("a" → 1, "b" → Obj.from("x" → 2)) shouldBe Obj.from("a" → 1, "b" → Obj.from("x" → 2))
    Obj.from("a" → 1, "b" → 3) + Obj.from("b" → 2) shouldBe Obj.from("a" → 1, "b" → 5)

    Obj.from("a" → 1) - Null shouldBe Obj.from("a" → 1)
    Null - Obj.from("a" → 1) shouldBe Null

    Obj.from("a" → 1, "b" → 2) - "b" shouldBe Obj.from("a" → 1)
    Obj.from("a" → 1, "b" → 2) - Obj.from("b" → 2) shouldBe Obj.from("a" → 1)
    Obj.from("a" → 1).contains("a") shouldBe true
    Obj.from("a" → 1).contains("b") shouldBe false
    Obj.from("a" → Some(1)).contains("a") shouldBe true
    val o: Option[String] = None
    Obj.from("a" → o).contains("a") shouldBe true

    // patch operation
    Obj.from("a" → 1) % Obj.from("b" → 2) shouldBe Obj.from("a" → 1, "b" → 2)
    Obj.from("a" → 1) % Null shouldBe Obj.empty
    Null % Obj.from("a" → 1) shouldBe Obj.from("a" → 1)
    Obj.from("a" → 1, "b" → Obj.from("x" → 2)) % Obj.from("b" → Null) shouldBe Obj.from("a" → 1, "b" → Null)
    Obj.from("b" → Null) % Obj.from("a" → 1, "b" → Obj.from("x" → 2)) shouldBe Obj.from("a" → 1, "b" → Obj.from("x" → 2))
    Obj.from("a" → 1, "b" → 3) % Obj.from("b" → 2) shouldBe Obj.from("a" → 1, "b" → 2)
  }

  "Bool operators " should "work" in {
    True > False shouldBe true
    !True shouldBe False
    !False shouldBe True
    True > False shouldBe true
    True < False shouldBe false
    True >= False shouldBe true
    True <= False shouldBe false
    True >= True shouldBe true
    True <= True shouldBe true
    True & True shouldBe True
    True | False shouldBe True
    True ^ True shouldBe False
    True & 0 shouldBe False
    False & 1 shouldBe False
    True & 1 shouldBe True
    False | 1 shouldBe True
  }

  "Annotated fields " should " preserve name" in {
    val p = TestProductAnnotated(576, 90)
    val v = p.toValue
    v.dynamic.f1Value should equal(Number(576))
    v.dynamic.f2Value should equal(Number(90))
    val p2 = v.to[TestProductAnnotated]
    p2 should equal(p)
  }

  "Value.removeNulls" should "remove Null fields eand items" in {
    Value.removeNullFields(Obj.from("a" → 1, "b" → Null, "c" → Lst.from(1,Null,2))) shouldBe Obj.from("a" → 1, "c" → Lst.from(1,Null,2))
  }

  def toValueNumberPair(kv: (String, Int)) = {
    (kv._1, Number(kv._2))
  }
}
