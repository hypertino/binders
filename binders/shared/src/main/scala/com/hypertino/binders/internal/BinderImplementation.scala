package com.hypertino.binders.internal

import com.hypertino.binders.core.{ImplicitDeserializer, ImplicitSerializer}
import com.hypertino.binders.value.Value
import com.hypertino.inflector.naming.Converter

import scala.collection.SeqLike
import scala.language.experimental.macros
import scala.language.reflectiveCalls
import scala.reflect.macros.Context
import scala.util.control.NonFatal

private [binders] trait BinderImplementation {
  val c: Context

  import c.universe._

  def bind[S: c.WeakTypeTag , O: c.WeakTypeTag](value: c.Tree): c.Tree = {
    val serOps = fresh("serOps")

    val customSerializer = c.inferImplicitValue(weakTypeOf[ImplicitSerializer[O,_]])
    val block =
    if (!customSerializer.isEmpty) {
      //c.universe.build.freshTypeName()

      //val ident =
      q"""{
        val $serOps = ${c.prefix.tree}
        $customSerializer.write($serOps.serializer, $value)
        $serOps.serializer
      }"""
    }
    else {
      val writers = extractWriters[S]
      val tpe = weakTypeOf[O]
      val writer = findWriter(writers, tpe)

      if (writer.isEmpty) {
        if (tpe <:< typeOf[Option[_]]){
          bindOption[S, O](value)
        }else
        if (tpe <:< typeOf[Either[_,_]]){
          bindEither[S, O](value)
        }else
        if (tpe <:< typeOf[Map[_,_]]){
          bindMap(value)
        }
        else if (tpe <:< typeOf[TraversableOnce[_]] || tpe <:< typeOf[Array[_]]){
          bindTraversable[S, O](value)
        }
        else if (tpe.typeSymbol.companionSymbol != NoSymbol){
          bindObject[S, O](value, partial = false)
        }
        else
          c.abort(c.enclosingPosition, s"No write function found for parameter with type $tpe in ${weakTypeOf[S]}")
      }
      else {
        q"""{
          val $serOps = ${c.prefix.tree}
          ${makeReaderWriterCall(q"$serOps.serializer", writer.get, List(value))}
          $serOps.serializer
          }"""
      }
    }

    //println(block)
    block
  }

  def bindArgs[S: c.WeakTypeTag](args: Seq[c.Tree]): c.Tree = {
    val serOps = fresh("serOps")
    val bindList = args.map(arg => q"$serOps.serializer.bind($arg)")
    val block = q"""{
      val $serOps = ${c.prefix.tree}
      ${callIfExists[S](q"$serOps.serializer", "beginArgs")}
      ..$bindList
      ${callIfExists[S](q"$serOps.serializer", "endArgs")}
      $serOps.serializer
      }"""

    // println(block)
    block
  }

  def bindOption[S: c.WeakTypeTag, O: c.WeakTypeTag](value: c.Tree): c.Tree = {
    val serOps = fresh("serOps")
    val block = q"""{
      val $serOps = ${c.prefix.tree}
      $value.map($serOps.serializer.bind(_)).getOrElse {
        $serOps.serializer.writeNull
      }
      $serOps.serializer.serializer
      }"""
    //println(block)
    block
  }

  def bindEither[S: c.WeakTypeTag, O: c.WeakTypeTag](value: c.Tree): c.Tree = {
    val serOps = fresh("serOps")
    val left = fresh("left")
    val right = fresh("right")
    val block = q"""{
      val $serOps = ${c.prefix.tree}
      $value match {
        case Left($left) => $serOps.serializer.bind($left)
        case Right($right) => $serOps.serializer.bind($right)
      }
      $serOps.serializer.serializer
      }"""
    //println(block)
    block
  }

  def bindObject[S: c.WeakTypeTag, O: c.WeakTypeTag](value: c.Tree, partial: Boolean): c.Tree = {
    val serOps = fresh("serOps")
    val o = fresh("o")
    val converter = findConverter[S]
    val caseClassParams = extractCaseClassParams[O]

    val listOfCalls: List[Tree] = caseClassParams.map { parameter =>
      val fieldName = identToFieldName(parameter, converter)
      val q =
        if (partial)
          q"$serOps.serializer.getFieldSerializer($fieldName).map(_.bind($o.${newTermName(parameter.name.toString)}))"
        else
          q"getFieldOrThrow($serOps.serializer.getFieldSerializer($fieldName), $fieldName).bind($o.${newTermName(parameter.name.toString)})"
      if (parameter.typeSignature <:< typeOf[Option[_]]
        || parameter.typeSignature <:< typeOf[Value]
        || parameter.typeSignature <:< typeOf[Iterable[_]])
        q"if (!$o.${newTermName(parameter.name.toString)}.isEmpty || !com.hypertino.binders.core.BindOptions.get.skipOptionalFields){$q}"
      else
        q
    }

    val block = q"""{
      import com.hypertino.binders.internal.Helpers._
      val $serOps = ${c.prefix.tree}
      val $o = $value
      ${callIfExists[S](q"$serOps.serializer", "beginObject")}
      ..$listOfCalls
      ${callIfExists[S](q"$serOps.serializer", "endObject")}
      $serOps.serializer
      }"""
    //println(block + " partial = " + partial)
    block
  }

  def bindTraversable[S : c.WeakTypeTag, O: c.WeakTypeTag](value: c.Tree): c.Tree = {
    val serOps = fresh("serOps")
    val block = q"""{
      val $serOps = ${c.prefix.tree}
      ${callIfExists[S](q"$serOps.serializer", "beginArray")}
      $value.foreach($serOps.bind(_))
      ${callIfExists[S](q"$serOps.serializer", "endArray")}
      $serOps.serializer
    }"""
    //println(block)
    block
  }

  def bindMap(value: c.Tree): c.Tree = {
    val serOps = fresh("serOps")
    val k = fresh("k")
    val v = fresh("v")
    val block = q"""{
      val $serOps = ${c.prefix.tree}
      $serOps.serializer.beginObject()
      $value.foreach{case ($k,$v) => {
        $serOps.serializer.getFieldSerializer($k).map(_.bind($v))
      }}
      $serOps.serializer.endObject()
      $serOps.serializer
    }"""
    //println(block)
    block
  }

  def unbind[D: c.WeakTypeTag, O: c.WeakTypeTag](partial: Boolean, originalValue: c.Tree): c.Tree = {
    val dserOps = fresh("dserOps")
    val customDeserializer = c.inferImplicitValue(weakTypeOf[ImplicitDeserializer[O, _]])
    //println(customDeserializer)
    val block =
      if (!customDeserializer.isEmpty) {
        q"""{
          val $dserOps = ${c.prefix.tree}
          $customDeserializer.read($dserOps.deserializer)
        }"""
      }
      else {
        val tpe = weakTypeOf[O]
        val readers = extractReaders[D]
        //println ("readers: " + readers)
        val reader = findReader(readers, tpe)
        //println("reader: " + reader)
        reader.map { readerMethod =>
          q"""{
            val $dserOps = ${c.prefix.tree}
            ${makeReaderWriterCall(q"$dserOps.deserializer", readerMethod)}
          }"""
        } getOrElse {
          val companionType = tpe.typeSymbol.companionSymbol.typeSignature
          companionType.declaration(newTermName("unapply")) match {
            case NoSymbol =>
              if (tpe <:< typeOf[Option[_]]) {
                unbindOption[D, O]
              } else
              if (tpe <:< typeOf[Either[_, _]]) {
                unbindEither[D, O]
              } else
              if (tpe <:< typeOf[Map[_, _]]) {
                unbindMap[O]
              }
              else if (tpe <:< typeOf[TraversableOnce[_]] || tpe <:< typeOf[Array[_]]) {
                unbindIterable[D, O]
              }
              else {
                c.abort(c.enclosingPosition, s"No read function found for $tpe in ${weakTypeOf[D]}")
              }
            case s => unbindObject[D,O](partial, originalValue)
          }
        }
      }

    block
  }

  def unbindOption[D: c.WeakTypeTag, O: c.WeakTypeTag]: c.Tree = {
    val dserOps = fresh("dserOps")
    val tpe = weakTypeOf[O]
    val elTpe = extractTypeArgs(tpe).head

    val block = q"""{
      val $dserOps = ${c.prefix.tree}
      if ($dserOps.deserializer.isNull)
        None
      else
        Some($dserOps.deserializer.unbind[$elTpe])
    }"""
    //println(block)
    block
  }

  def unbindEither[D: c.WeakTypeTag, O: c.WeakTypeTag]: c.Tree = {
    val dserOps = fresh("dserOps")
    val v = fresh("v")
    val leftIsBetter = fresh("leftIsBetter")
    val r = fresh("r")
    val r1 = fresh("r1")
    val r2 = fresh("r2")
    val e1 = fresh("e1")
    val e2 = fresh("e2")
    val tpe = weakTypeOf[O]
    val left = extractTypeArgs(tpe).head
    val right = extractTypeArgs(tpe).tail.head

    val leftDStr = getTypeDynamicString(left.tpe)
    val rightDStr = getTypeDynamicString(right.tpe)

    val block = q"""{
      val $dserOps = ${c.prefix.tree}
      import com.hypertino.binders.value._
      import scala.util._
      val $v = $dserOps.deserializer.unbind[com.hypertino.binders.value.Value]
      val $leftIsBetter = com.hypertino.binders.internal.Helpers.getConformity($leftDStr,$v) >=
        com.hypertino.binders.internal.Helpers.getConformity($rightDStr,$v)

      val $r = Try (if ($leftIsBetter) Left($v.fromValue[$left]) else Right($v.fromValue[$right]))
        match {
          case Success($r1) => $r1
          case Failure($e1) =>
            Try (if ($leftIsBetter) Right($v.fromValue[$right]) else Left($v.fromValue[$left]))
            match {
              case Success($r2) => $r2
              case Failure($e2) =>
                throw new com.hypertino.binders.core.BindersException("Value '"+$v+"' didn't match neither Left nor Right", $e2)
            }
        }
      $r
    }"""
    //println(block)
    block
  }

  def getTypeDynamicString(ct: Type) = {
    val t = if (ct <:< typeOf[Option[_]]) extractTypeArgs(ct).head.tpe else ct

    if (t =:= typeOf[Double]
      || t =:= typeOf[Float]
      || t =:= typeOf[Int]
      || t =:= typeOf[Long]
      || t =:= typeOf[Byte]
      || t =:= typeOf[Short]) {
      "Number"
    }else
    if (t =:= typeOf[String]) {
      "Text"
    }else
    if (t =:= typeOf[Boolean]) {
      "Bool"
    }else
    if (t <:< typeOf[SeqLike[_,_]]) {
      "Lst"
    }
    else {
      "Obj"
    }
    //Iterable
  }

  def unbindIterable[D: c.WeakTypeTag, O: c.WeakTypeTag]: c.Tree = {
    val dserOps = fresh("dserOps")
    val tpe = weakTypeOf[O]
    val elTpe = extractTypeArgs(tpe).head

    q"""{
      val $dserOps = ${c.prefix.tree}
      ${convertIterator(tpe, q"$dserOps.deserializer.iterator().map(_.unbind[$elTpe])")}
    }"""
  }

  def unbindObject[D: c.WeakTypeTag, O: c.WeakTypeTag](partial: Boolean, originalValue: c.Tree): c.Tree = {
    val dserOps = fresh("dserOps")
    val i = fresh("i")
    val orig = fresh("orig")
    val converter = findConverter[D]
    val caseClassParams = extractCaseClassParams[O]
    val companionSymbol = weakTypeOf[O].typeSymbol.companionSymbol

    val vars = caseClassParams.zipWithIndex.map { case (parameter, index) =>
      val varName = newTermName("i_" + parameter.name.decoded)
      val fieldName = identToFieldName(parameter, converter)

      (
        // _1
        if (partial)
          q"var $varName : Option[${parameter.typeSignature}] = Some($orig.${newTermName(parameter.name.decoded)})"
        else
          q"var $varName : Option[${parameter.typeSignature}] = None",

        // _2
        if (parameter.asTerm.isParamWithDefault) {
          cq"""$fieldName => {
            $varName = $i.unbind[Option[${parameter.typeSignature}]]
          }"""
        } else {
          cq"""$fieldName => {
            $varName = Some($i.unbind[${parameter.typeSignature}])
          }"""
        },

        // _3
        if (parameter.asTerm.isParamWithDefault) {
          val defVal = newTermName("apply$default$" + (index + 1))
          q"$parameter = $varName.getOrElse($companionSymbol.$defVal)"
        } else if (parameter.typeSignature <:< typeOf[Option[_]])
          q"$parameter = $varName.flatten"
        else if (parameter.typeSignature <:< typeOf[Value])
          q"$parameter = $varName.getOrElse(com.hypertino.binders.value.Null)"
        else if (parameter.typeSignature <:< typeOf[Map[_,_]])
          q"$parameter = $varName.getOrElse(Map.empty)"
        else if (parameter.typeSignature <:< typeOf[Vector[_]])
          q"$parameter = $varName.getOrElse(Vector.empty)"
        else if (parameter.typeSignature <:< typeOf[IndexedSeq[_]])
          q"$parameter = $varName.getOrElse(IndexedSeq.empty)"
        else if (parameter.typeSignature <:< typeOf[Set[_]])
          q"$parameter = $varName.getOrElse(Set.empty)"
        else if (parameter.typeSignature <:< typeOf[List[_]])
          q"$parameter = $varName.getOrElse(List.empty)"
        else if (parameter.typeSignature <:< typeOf[Seq[_]])
          q"$parameter = $varName.getOrElse(Seq.empty)"
        else if (parameter.typeSignature <:< typeOf[Array[_]])
          q"$parameter = $varName.getOrElse(Array())"
        else
          q"$parameter = $varName.getOrElse(throw new com.hypertino.binders.core.FieldNotFoundException($fieldName))"
      )
    }

    val block = q"""{
      val $dserOps = ${c.prefix.tree}
      ${if (partial) { q"val $orig = $originalValue" } else q""}
      ..${vars.map(_._1)}
      $dserOps.deserializer.iterator().foreach{case $i =>
        $i.fieldName.map {
          case ..${vars.map(_._2)}
          case _ => { /*todo: implement smart deserialization*/ }
        } getOrElse {
          throw new com.hypertino.binders.core.BindersException("Can't deserialize object: iterator didn't return fieldName")
        }
      }

      $companionSymbol(
        ..${vars.map(_._3)}
      )
    }"""
    //println(block)
    block
  }

  def unbindMap[O: c.WeakTypeTag]: c.Tree = {
    val dserOps = fresh("dserOps")
    val el = fresh("el")
    val tpe = weakTypeOf[O]
    val elTpe = extractTypeArgs(tpe).tail.head
    val block = q"""{
      val $dserOps = ${c.prefix.tree}
      $dserOps.deserializer.iterator().map{ case $el =>
        ($el.fieldName.get, $el.unbind[$elTpe])
      }.toMap
    }"""
    //println(block)
    block
  }

  protected def convertIterator(ct: Type, iteratorTree: Tree): Tree = {
    val selector: Option[String] =
      if (ct <:< typeOf[Vector[_]]) {
        Some("toVector")
      }else
      if (ct <:< typeOf[List[_]]) {
        Some("toList")
      }else
      if (ct <:< typeOf[IndexedSeq[_]]) {
        Some("toIndexedSeq")
      }else
      if (ct <:< typeOf[Set[_]]) {
        Some("toSet")
      }else
      if (ct <:< typeOf[Seq[_]]) {
        Some("toList") // don't use toSeq which creates lazy stream sequence
      }else
      if (ct <:< typeOf[Array[_]]) {
        Some("toArray")
      }
      else
        None
    selector.map { s =>
      if (ct <:< typeOf[Set[_]]) // toSet needs also TypeApply
        TypeApply(Select(iteratorTree, newTermName(s)), extractTypeArgs(ct))
      else
        Select(iteratorTree, newTermName(s))
    } getOrElse {
      iteratorTree
    }
  }

  protected def extractTypeArgs(tpe: Type): List[TypeTree] = {
    tpe.normalize match {
      case TypeRef(_, _, args) => args.map(TypeTree(_))
      case _ =>
        c.abort(c.enclosingPosition, s"Can't extract typeArgs from $tpe")
    }
  }

  protected def applyTypeArgs(select: Select, srcTypeArgs: Map[Symbol, Type], dstTypeParams: List[Symbol]) = {
    // println("typeArgs == " + srcTypeArgs + " dstTypes == " + dstTypeParams)
    if (srcTypeArgs.isEmpty || dstTypeParams.isEmpty)
      select
    else {
      TypeApply(select,
        dstTypeParams.map { genericTypeSymbol =>
          srcTypeArgs.get(genericTypeSymbol).map { srcTypeArg =>
            TypeTree(srcTypeArg)
          } getOrElse {
            c.abort(c.enclosingPosition, "Can't find generic arg source for " + select + " / " + genericTypeSymbol)
          }
        })
    }

  }

  protected def makeReaderWriterCall(elemTerm: Tree, method: (MethodSymbol, Map[Symbol, Type]), parameters: List[Tree] = List()): Apply = {
    val inner = Apply(applyTypeArgs(Select(elemTerm, method._1),  method._2,  method._1.typeParams), parameters)
    if (method._1.paramss.isEmpty)
      inner
    else
      method._1.paramss.tail.foldLeft(inner) { (a: Apply, params: List[Symbol]) =>
        Apply(a,
          params.map(p =>
            Select(
              Select(Ident(newTermName("scala")), newTermName("Predef")),
              newTermName("implicitly")
            )
          )
        )
      }
  }

  protected def mostMatching(methods: List[MethodSymbol], scoreFun: MethodSymbol => Option[(Int, Map[Symbol, Type])]): Option[(MethodSymbol, Map[Symbol, Type])] = {
    var rMax: Int = 0
    var mRes: Option[(MethodSymbol,Map[Symbol, Type])] = None
    methods.foreach({ m => // todo: replace to .max and remove vars
      scoreFun(m) match {
        case Some((r, typeArgs)) =>
          if (r > rMax) {
            rMax = r
            mRes = Some(m, typeArgs)
          }
        case None => // do nothing
      }
      // println("Comparing " + m + " with arg type " + methodParSym.typeSignature + " for parameter " + parSym + " with type " + parSymType + " RES = " + r + " --- " + math.random)
    })
    mRes
  }

  protected def findWriter(writers: List[MethodSymbol], valueType: Type/*, print: Boolean = false*/): Option[(MethodSymbol, Map[Symbol, Type])] = {
    mostMatching(writers, m => {
      val writerType = m.paramss.head.head // parSym 0 - value
      Some(compareTypes(valueType, writerType.typeSignature/*, print*/, m.typeParams))
    })
  }

  protected def compareTypes(left: Type, right: Type, typeParams: List[Symbol]): (Int, Map[Symbol, Type]) = {
    if (left =:= right)
      (100, Map.empty)
    else
    if (left <:< right)
      (90, Map.empty)
    else
    if (left weak_<:< right)
      (80, Map.empty)
    else {
      right match {
        case TypeRef(rightTpe, rightSym, rightArgs) => {
          val typeMap = collection.mutable.Map[Symbol, Type]()
          var r =
            if (left.typeSymbol.typeSignature =:= right.typeSymbol.typeSignature) // Outer type is matched fully
              50
            else
            if (left.baseClasses.exists(_.typeSignature =:= right.typeSymbol.typeSignature)) // Outer type inherits
              30
            else
            if (rightTpe == NoPrefix) {
              // Right symbol is generic type parameter
              typeParams.find(_ == rightSym).map{ typeParamSymbol ⇒
                val typeMatched = typeParamSymbol.typeSignature match {
                  case TypeBounds(lo,hi) ⇒
                    lo <:< left && left <:< hi
                  case other: Type ⇒
                    left <:< other
                }
                if (typeMatched) {
                  typeMap += rightSym -> left
                  20
                }
                else
                  0
              } getOrElse {
                0
              }
            }
            else
              0

          if (r > 0) {
            left match {
              case TypeRef(leftTpe, leftSym, leftArgs) => {
                // now check generic type args
                if (leftArgs.size == rightArgs.size) {
                  for (i <- leftArgs.indices) {
                    val lefT = leftArgs(i)
                    val rightT = rightArgs(i)
                    val tR = compareTypes(lefT, rightT, typeParams)
                    if (tR._1 != 0) {
                      typeMap ++= tR._2
                    }
                    else
                      r = 0
                  }
                }/* else {
                  r = 0
                }*/
              }
              /*case RefinedType(_) ⇒
                if (rightArgs.nonEmpty)
                  r = 0
              case _ =>
                r = 0*/
              case _ ⇒ // do nothing
            }
          }
          (r, typeMap.toMap)
        }
        case _ => (0, Map.empty)
      }
    }
  }

  protected def callIfExists[S: c.WeakTypeTag](o: c.Tree, methodName: String): c.Tree = {
    weakTypeOf[S].members.filter(member => member.isMethod &&
      member.name.decoded == methodName &&
      member.isPublic && {
      val m = member.asInstanceOf[MethodSymbol]
      //println("method: " + member.name.decoded + " params: " + m.paramss)
      m.paramss.isEmpty ||
        (m.paramss.size == 1 && allImplicits(List(m.paramss.head))) ||
        (m.paramss.size == 2 && m.paramss.head.isEmpty && allImplicits(m.paramss.tail))
    }
    ).map(_.asInstanceOf[MethodSymbol]).headOption.map { m =>
      q"$o.${newTermName(methodName)}()"
    } getOrElse {
      q""
    }
  }

  protected def extractWriters[T: c.WeakTypeTag]: List[MethodSymbol] = {
    weakTypeOf[T].members.filter(member => member.isMethod &&
      member.name.decoded.startsWith("write") &&
      member.isPublic && {
        val m = member.asInstanceOf[MethodSymbol]
        m.paramss.nonEmpty &&
          (m.paramss.tail.isEmpty || allImplicits(m.paramss.tail)) &&
          m.paramss.head.size == 1 // only 1 parameter
      }
    ).map(_.asInstanceOf[MethodSymbol]).toList
  }

  protected def findReader(readers: List[MethodSymbol], returnType: Type): Option[(MethodSymbol, Map[Symbol, Type])] = {
    mostMatching(readers, m => {
      Some(compareTypes(returnType, m.returnType, m.typeParams))
    })
  }

  protected def extractReaders[T: c.WeakTypeTag]: List[MethodSymbol] = {
    weakTypeOf[T].members.filter(member => member.isMethod &&
      member.name.decoded.startsWith("read") &&
      member.isPublic && {
        val m = member.asInstanceOf[MethodSymbol]
        //println("method: " + member.name.decoded + " params: " + m.paramss)
        m.paramss.isEmpty ||
          (m.paramss.size == 1 && allImplicits(List(m.paramss.head))) ||
          (m.paramss.size == 2 && m.paramss.head.isEmpty && allImplicits(m.paramss.tail))
      }
    ).map(_.asInstanceOf[MethodSymbol]).toList
  }

  protected def allImplicits(symbols: List[List[Symbol]]): Boolean = symbols.flatten.filter(!_.isImplicit).isEmpty

  protected def extractCaseClassParams[T: c.WeakTypeTag]: List[c.Symbol] = {

    val companioned = weakTypeOf[T].typeSymbol
    val companionSymbol = companioned.companionSymbol
    val companionType = companionSymbol.typeSignature

    companionType.declaration(newTermName("unapply")) match {
      case NoSymbol => c.abort(c.enclosingPosition, s"No setter or unapply function found for ${companioned.fullName}")
      case s =>
        val unapply = s.asMethod
        val unapplyReturnTypes = unapply.returnType match {
          case TypeRef(_, _, Nil) =>
            c.abort(c.enclosingPosition, s"Apply of ${companionSymbol} has no parameters. Are you using an empty case class?")
          case TypeRef(_, _, args) =>
            args.head match {
              case t@TypeRef(_, _, Nil) => Some(List(t))
              case t@TypeRef(_, _, args) =>
                if (t <:< typeOf[Option[_]]) Some(List(t))
                else if (t <:< typeOf[Seq[_]]) Some(List(t))
                else if (t <:< typeOf[Set[_]]) Some(List(t))
                else if (t <:< typeOf[Array[_]]) Some(List(t))
                else if (t <:< typeOf[Map[_, _]]) Some(List(t))
                else if (t <:< typeOf[Product]) Some(args)
              case _ => None
            }
          case _ => None
        }

        companionType.declaration(newTermName("apply")) match {
          case NoSymbol => c.abort(c.enclosingPosition, "No apply function found")
          case s =>
            // searches apply method corresponding to unapply
            val applies = s.asTerm.alternatives
            val apply = applies.collectFirst {
              case (apply: MethodSymbol) if (apply.paramss.headOption.map(_.map(_.asTerm.typeSignature)) == unapplyReturnTypes) => apply
            }
            apply match {
              case Some(apply) =>

                // println("apply found:" + apply)
                if (!apply.paramss.tail.isEmpty)
                  c.abort(c.enclosingPosition, "Couldn't use apply method with more than a single parameter group")

                apply.paramss.head

              case None => c.abort(c.enclosingPosition, "No apply function found matching unapply parameters")
            }
        }

    }
  }

  protected def identToFieldName(symbol: c.Symbol, converter: Option[Converter]): Literal = {

    val annotation = symbol.annotations.find(a => a.tpe == typeOf[com.hypertino.binders.annotations.fieldName])
    val (fieldName,useConverter) = annotation.map { a =>
      /*a.scalaArgs.foreach(x =>
        println(s"${x.getClass}/$x")
      )*/
      (a.scalaArgs.head match {
        case Literal(Constant(s:String)) => s
        case _ => symbol.name.decoded
      },
        a.scalaArgs.tail.head match {
        case Literal(Constant(b:Boolean)) => b
        case _ => false
      })
    } getOrElse {
      (symbol.name.decoded, true)
    }
    //println(s"anno: $fieldName $useConverter")
    Literal(Constant(
      converter.map { c =>
        if (useConverter)
          c.convert(fieldName)
        else
          fieldName
      } getOrElse {
        fieldName
      }
    ))
  }

  protected def findConverter[T: c.WeakTypeTag]: Option[Converter] = {
    val tpe = weakTypeOf[T]
    val converterTypeName = newTypeName("nameConverterType")

    val converterTypeOption = tpe.baseClasses.flatMap {
      baseSymbol =>
        val baseType = tpe.baseType(baseSymbol)
        val ct = baseType.declaration(converterTypeName)
        ct match {
          case NoSymbol => None
          case _ =>
            val t = ct.typeSignature.asSeenFrom(tpe, baseSymbol)
            t.baseClasses.find(t.typeSymbol.isClass && _ == typeOf[Converter].typeSymbol).map { x =>
              t.typeSymbol.asClass
            } orElse {
              c.abort(c.enclosingPosition, s"$tpe.nameConverterType: ${t} is not a valid Converter, please use PlainConverter if you don't need convert identifier names")
            }
        }
    }.headOption

    converterTypeOption map { t =>
      // this is synchronized because of bug in scala
      // http://docs.scala-lang.org/overviews/reflection/thread-safety.html
      this.synchronized {
        val ru = scala.reflect.runtime.universe

        // todo: there should a better way to get runtime-symbol from compile-time
        val clz = Class.forName(t.fullName)
        val mirror = ru.runtimeMirror(getClass.getClassLoader)

        try {
          val moduleSymbol = mirror.moduleSymbol(clz)
          val module = mirror.reflectModule(moduleSymbol)
          module.instance.asInstanceOf[Converter]
        }
        catch {
          case NonFatal(e) ⇒
            val sym = mirror.classSymbol(clz)
            val r = mirror.reflectClass(sym)
            val m = r.symbol.typeSignature.member(ru.nme.CONSTRUCTOR).asMethod
            val ctr = r.reflectConstructor(m)
            ctr().asInstanceOf[Converter]
        }
      }
    }
  }

  def fresh(prefix: String): TermName = newTermName(c.fresh(prefix))
}
