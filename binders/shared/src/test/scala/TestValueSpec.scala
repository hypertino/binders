import com.hypertino.binders.value._
import org.scalatest._
import ValueBinders._

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

    val m = ObjV("a" -> "he", "b" -> "ho")
    val map = m.to[StringMap]
    map should equal(Map("a"->"he", "b"->"ho"))
  }

  "Value " should " allow selectDynamic " in {
    val d = ObjV("a" -> 1, "b" -> "ho", "c" -> true, "_" -> false,
      "inner" → ObjV("x" → "100500")
    )
    val a = d.a
    a should equal(Number(1))

    val b = d.b
    b should equal(Text("ho"))

    val f = d.__
    f should equal(Bool(false))

    //val i = d.inner.x // todo: this doesn't work under scala 2.10, uncomment in the future
    //i shouldBe Text("100500")
  }

  "Obj " should " merge (+) " in {
    val value1 = ObjV("a" -> 1, "b" -> "ho", "c" -> true, "d" → 5, "e" → "kl")
    val value2 = ObjV("a" -> 2, "b" -> "no", "c" -> false, "d" → Null)
    val value3 = value1 + value2
    value3 should equal(ObjV("a" -> 2, "b" -> "no", "c" -> false, "d" → Null, "e" → Text("kl")))
  }

  "Obj " should " subtract (-) " in {
    val value1 = ObjV("a" -> 1, "b" -> "ho", "c" -> true, "d" → 5, "e" → "kl")
    val value2 = ObjV("a" -> Null, "d" → 8)
    val value3 = value1 - value2
    value3 should equal(ObjV("b" -> "ho", "c" -> true, "e" → Text("kl")))
  }

  "Obj " should " preserve order of fields " in {
    val seq = Seq[(String, Value)]("a" -> 1, "b" -> "ho", "c" -> true, "d" → 5, "e" → "kl")
    val value1 = ObjV(seq: _*)

    val z = seq.zipWithIndex.map(a ⇒ a._2 → a._1).toMap

    value1.v.zipWithIndex.foreach{
      case (kv, index) ⇒
        z(index) should equal(kv)
    }
  }

  "implicits" should "do conversion" in {
    val obj = ObjV("a" → 5, "b" → "18")
    obj should equal(Obj(Map("a"→Number(5), "b"→Text("18"))))

    val obj2 = ObjV("a" → "b")
    obj2 should equal(Obj(Map("a"→Text("b"))))

    val lst = Lst(Seq("a",1,false))
    lst should equal(Lst(Seq(Text("a"),Number(1),Bool(false))))
  }

  "Value " should "do pattern matching" in {
    val obj = ObjV("a" → 5, "b" → "18")
    obj match {
      case Obj(map) ⇒ // fine
    }

    val lst: Value = LstV("a",1,false)
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
    LstV(1,2,3) ++ LstV(4,5) shouldBe LstV(1,2,3,4,5)
    LstV(1,2,3) + 4 shouldBe LstV(1,2,3,4)
    LstV(1,2,3) -- LstV(2) shouldBe LstV(1,3)
    LstV(1,2,3) - 2 shouldBe LstV(1,3)
    LstV(1,2,3).contains(2) shouldBe true
    LstV(1,2,3).contains(4) shouldBe false
  }

  "Obj operators " should "work" in {
    ObjV("a" → 1).contains("a") shouldBe true
    ObjV("a" → 1).contains("b") shouldBe false
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


  def toValueNumberPair(kv: (String, Int)) = {
    (kv._1, Number(kv._2))
  }
}
