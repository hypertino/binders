[![Build Status](https://travis-ci.org/hypertino/binders.svg)](https://travis-ci.org/hypertino/binders)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.hypertino/binders_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.hypertino/binders_2.12)
[![Join the chat at https://gitter.im/Hypertino/binders](https://badges.gitter.im/Hypertino/binders.svg)](https://gitter.im/Hypertino/binders?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

# About

_binders_ is a Scala/Scala.js library for creating serialization/mapping libraries and frameworks. 

The aim of _binders_ 
is to allow easily make fast (compile-time/macro based) serialization library for case-classes, collections, primitives. 
It takes out most of the complexity dealing with macro.

Another thing provided by _binders_ is a schema-less (untyped) data represented by `Value` type.

Existing examples of serialization libraries based on binders are:
- [json-binders](https://github.com/hypertino/json-binders) very fast and flexible JSON serializer for Scala/Scala.js
- [typesafe-config-binders](https://github.com/hypertino/typesafe-config-binders) easily read typesafe config directly to case classes 
- [cassandra-binders](https://github.com/hypertino/cassandra-binders) case/class mapping for cassandra to minimize boilerplate code

# Usage

## Serialization

Implement trait `Serializer[C]` and provide methods with names starting `write` and accepts single argument with data 
type that is serialized. Also, to serialize classes serializer should provide a method `getFieldSerializer`

```scala
import com.hypertino.binders.core.Serializer
import com.hypertino.inflector.naming._

class MySerializer[C <: Converter] extends Serializer[C] {
  def writeInt(i: Int) = println(i)
  def writeString(s: String) = println(s)
  def writeNull = println("null")

  def getFieldSerializer(fieldName: String): Option[MySerializer[C]] = {
    println("serializing:" + fieldName)
    Some(new MySerializer[C])
  }
}
```
That's all. With this code you may "serialize" case-classes, collections, primitive types (int and string in this case).

```scala
val serializer = new MySerializer[PlainConverter.type]
serializer.bind(A(10, "hello", None))  // case class
serializer.bind(10) //primitive 
serializer.bind(Seq(1,2,3)) // collection
```

You may also want to implement methods `beginObject`/`endObject` and `beginArray`/`endArray`. _binders_ will call these 
methods according to the type provided to call `bind`

> `PlainConverter.type` is a converter of field-names that do nothing. There are more useful converters like `SnakeCaseToCamelCaseConverter`,
`DashCaseToCamelCaseConverter`. They are used to transform field names according to the naming style we need.  

More detailed example of serializer in json-binders is [JSON serialiser](https://github.com/hypertino/json-binders/blob/master/jsonBinders/shared/src/main/scala/com/hypertino/binders/json/JsonSerializer.scala) 

## Deserialization

Implement trait `Deserializer[C]` and provide methods for reading primitives and other data-types. Example:

```scala
import com.hypertino.binders.core.Deserializer
import com.hypertino.inflector.naming._
class MyDeserializer[C <: Converter](val fieldName: Option[String] = None) extends Deserializer[C] {
  def readInt(): Int = 10
  def readString(): String = "hello"
  def isNull: Boolean = false

  def iterator(): Iterator[MyDeserializer[C]] = Seq("x","y","z").map(s ⇒ new MyDeserializer[C](Some(s))).toIterator
}
```

"deserializer" usage example:

```scala
val deserializer = new MyDeserializer[PlainConverter.type]
val a = deserializer.unbind[A] // = A(10,"Hello",Some(10))
val i = deserializer.unbind[Int] // = 10
val s = deserializer.unbind[String] // = "hello"
```

Full example of [JSON deserializer](https://github.com/hypertino/json-binders/blob/master/jsonBinders/shared/src/main/scala/com/hypertino/binders/json/JsonDeserializer.scala)

## Value type

You may use `Value` type for runtime manipulation or for the schema-less data, when it's know known at a compile time
exact type structure. `Value` type can be mixed into the typed structure if data is partially untyped. One of the
examples of this is extra properties when some class have attached additional, runtime-defined properties.  

`Value` type is a sealed trait that can only be one of the following types:

- `Obj` - an object/map structure
- `Lst` - list/array/sequence
- `Text` - string type
- `Number` - numbers internally represented as BigDecimal
- `Bool` - logical type
- `Null` - null object/value

Some examples:

```scala
// construct some Obj
val obj = Obj.from("a" -> Obj.from("x" → 1, "y" → "yey"))

// if we have case-class we can convert it to Obj:
case class A(x: Int, y: String)  
...
val obj = A(10, "hello").toValue // equals to Obj.from("x" -> 10, "y" -> "hello)

// we also can convert it back to case-class
val a = obj.to[A]
```

> `Value.dynamic` method is implemented using scala `Dynamic` type, so you can access fields by name even they 
> are not defined at compile-time 
> 
> ```scala
> val obj = Obj.from("a" -> Obj.from("x" → 1, "y" → "yey"))
> val a = obj.dynamic.a // = Text("a")
> ```


You may also find an example of mixing `Value` in a case-class [with json-binders](https://github.com/hypertino/json-binders#schemalesscustom-fields)

# Download

```sbt
libraryDependencies += "com.hypertino" %% "binders" % "1.2.0"
```
Releases published to Maven Central for Scala 2.10/2.11/2.12 JVM & JS (user `%%%` for Scala.js enabled projects)

Snapshots live in Sonatype repository, include it additionally:
```sbt
resolvers ++= Seq(
  Resolver.sonatypeRepo("public")
)
```

# License

_binders_ library is available under the BSD 3-Clause License


