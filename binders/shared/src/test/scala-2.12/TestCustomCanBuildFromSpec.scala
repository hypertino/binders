import com.hypertino.binders.value.{Lst, Null, Obj}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.generic.CanBuildFrom
import scala.collection.{TraversableLike, mutable}

class CustomColl[+A] extends Traversable[A] with TraversableLike[A, CustomColl[A]]{
  override protected[this] def newBuilder: mutable.Builder[A, CustomColl[A]] = CustomColl.newBuilder[A]
  override def foreach[U](f: (A) => U): Unit = {}
}

object CustomColl {
  def newBuilder[A]: mutable.Builder[A, CustomColl[A]] = new mutable.Builder[A, CustomColl[A]] {
    override def +=(elem: A): this.type = this
    override def clear(): Unit = {}
    override def result(): CustomColl[A] = new CustomColl[A]
  }

  implicit def canBuildFrom[A]: CanBuildFrom[Nothing, A, CustomColl[A]] = new CanBuildFrom[Nothing, A, CustomColl[A]] {
    override def apply(from: Nothing): mutable.Builder[A,CustomColl[A]] = newBuilder[A]
    override def apply(): mutable.Builder[A,CustomColl[A]] = newBuilder[A]
  }
}

case class CustomCollHolder(a: CustomColl[Int])


class TestCustomCanBuildFromSpec extends FlatSpec with Matchers with MockFactory {
  "custom CanBuildFrom[] " should " unbind" in {
    val lst = Lst.empty
    val customColl: CustomColl[Int] = lst.to[CustomColl[Int]]
    customColl shouldBe a[CustomColl[_]]
  }

  "custom CanBuildFrom[] " should " have default empty value" in {
    val obj = Obj.from("a" → Null)
    val customHolder = obj.to[CustomCollHolder]
    customHolder shouldBe a [CustomCollHolder]
    customHolder.a shouldBe a[CustomColl[_]]

    val obj2 = Null
    val customHolder2 = obj2.to[CustomCollHolder]
    customHolder2 shouldBe a [CustomCollHolder]
    customHolder2.a shouldBe a[CustomColl[_]]
  }
}
