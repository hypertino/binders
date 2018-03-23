package com.hypertino.binders.internal

import com.hypertino.binders.core.{ImplicitDeserializer, ImplicitSerializer}
import com.hypertino.binders.util.MacroAdapter.Context
import com.hypertino.binders.util.{MacroAdapter, Runtime}
import com.hypertino.binders.value.{Value, WithExtra}
import com.hypertino.inflector.naming.Converter

import scala.collection.SeqLike
import scala.language.experimental.macros

private [binders] trait BindersMacroImpl extends MacroAdapter[Context] {
  import ctx.universe._

  def bind[S: ctx.WeakTypeTag , O: ctx.WeakTypeTag](value: ctx.Tree): ctx.Tree = {
    val serOps = freshTerm("serOps")

    val customSerializer = ctx.inferImplicitValue(weakTypeOf[ImplicitSerializer[O,_]])
    val block =
    if (!customSerializer.isEmpty) {
      //ctx.universe.build.freshTypeName()
      //val ident =
      q"""{
        val $serOps = ${ctx.prefix.tree}
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
        else if (tpe.typeSymbol.companion != NoSymbol){
          bindObject[S, O](value, partial = false)
        }
        else
          ctx.abort(ctx.enclosingPosition, s"No write function found for parameter with type $tpe in ${weakTypeOf[S]}")
      }
      else {
        q"""{
          val $serOps = ${ctx.prefix.tree}
          ${makeReaderWriterCall(q"$serOps.serializer", writer.get, List(value))}
          $serOps.serializer
          }"""
      }
    }

    //println(block)
    block
  }

  def bindArgs[S: ctx.WeakTypeTag](args: Seq[ctx.Tree]): ctx.Tree = {
    val serOps = freshTerm("serOps")
    val bindList = args.map(arg => q"$serOps.serializer.bind($arg)")
    val block = q"""{
      val $serOps = ${ctx.prefix.tree}
      ${callIfExists[S](q"$serOps.serializer", "beginArgs")}
      ..$bindList
      ${callIfExists[S](q"$serOps.serializer", "endArgs")}
      $serOps.serializer
      }"""

    // println(block)
    block
  }

  def bindOption[S: ctx.WeakTypeTag, O: ctx.WeakTypeTag](value: ctx.Tree): ctx.Tree = {
    val serOps = freshTerm("serOps")
    val block = q"""{
      val $serOps = ${ctx.prefix.tree}
      $value.map($serOps.serializer.bind(_)).getOrElse {
        $serOps.serializer.writeNull
      }
      $serOps.serializer.serializer
      }"""
    //println(block)
    block
  }

  def bindEither[S: ctx.WeakTypeTag, O: ctx.WeakTypeTag](value: ctx.Tree): ctx.Tree = {
    val serOps = freshTerm("serOps")
    val left = freshTerm("left")
    val right = freshTerm("right")
    val block = q"""{
      val $serOps = ${ctx.prefix.tree}
      $value match {
        case Right($right) => $serOps.serializer.bind($right)
        case Left($left) => $serOps.serializer.bind($left)
      }
      $serOps.serializer.serializer
      }"""
    //println(block)
    block
  }

  def bindObject[S: ctx.WeakTypeTag, O: ctx.WeakTypeTag](value: ctx.Tree, partial: Boolean): ctx.Tree = {
    val serOps = freshTerm("serOps")
    val o = freshTerm("o")
    val converter = createConverter[S]
    val caseClassParams = extractCaseClassParams[O]
    val isExtraType = isWithExtra[O]

    val listOfCalls: List[Tree] = caseClassParams.map { parameter =>
      val fieldName = identToFieldName(parameter, converter)
      val q =
        if (partial)
          q"$serOps.serializer.getFieldSerializer($fieldName).map(_.bind($o.${TermName(parameter.name.toString)}))"
        else
          q"getFieldOrThrow($serOps.serializer.getFieldSerializer($fieldName), $fieldName).bind($o.${TermName(parameter.name.toString)})"
      if (parameter.typeSignature <:< typeOf[Option[_]]
        || parameter.typeSignature <:< typeOf[Value]
        || parameter.typeSignature <:< typeOf[Iterable[_]])
        q"if (!$o.${TermName(parameter.name.toString)}.isEmpty || !com.hypertino.binders.core.BindOptions.get.skipOptionalFields){$q}"
      else
        q
    }

    val bindExtra = if (isExtraType) {
      val kv = freshTerm("kv")
      val n = freshTerm("n")
      val convertedName = findConverter[S].map { converter =>
        q"""
          ${converter.termSymbol}.convert($kv._1)
        """
      } getOrElse {
        q"""
          $kv._1
        """
      }
      if (partial) {
        q"""
          $o.extra.v.foreach { case $kv =>
            $serOps.serializer.getFieldSerializer($convertedName).map(_.bind($kv._2))
          }
        """
      } else {
        q"""
          $o.extra.v.foreach { case $kv =>
            val $n = $convertedName
            getFieldOrThrow($serOps.serializer.getFieldSerializer($n), $n).bind($kv._2)
          }
        """
      }
    }
    else {
      EmptyTree
    }

    val block = q"""{
      import com.hypertino.binders.internal.Helpers._
      val $serOps = ${ctx.prefix.tree}
      val $o = $value
      ${callIfExists[S](q"$serOps.serializer", "beginObject")}
      ..$listOfCalls
      $bindExtra
      ${callIfExists[S](q"$serOps.serializer", "endObject")}
      $serOps.serializer
      }"""
    // println(block + " partial = " + partial)
    block
  }

  def bindTraversable[S : ctx.WeakTypeTag, O: ctx.WeakTypeTag](value: ctx.Tree): ctx.Tree = {
    val serOps = freshTerm("serOps")
    val block = q"""{
      val $serOps = ${ctx.prefix.tree}
      ${callIfExists[S](q"$serOps.serializer", "beginArray")}
      $value.foreach($serOps.bind(_))
      ${callIfExists[S](q"$serOps.serializer", "endArray")}
      $serOps.serializer
    }"""
    //println(block)
    block
  }

  def bindMap(value: ctx.Tree): ctx.Tree = {
    val serOps = freshTerm("serOps")
    val k = freshTerm("k")
    val v = freshTerm("v")
    val block = q"""{
      val $serOps = ${ctx.prefix.tree}
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

  def unbind[D: ctx.WeakTypeTag, O: ctx.WeakTypeTag](partial: Boolean, originalValue: ctx.Tree): ctx.Tree = {
    val dserOps = freshTerm("dserOps")
    val customDeserializer = ctx.inferImplicitValue(weakTypeOf[ImplicitDeserializer[O, _]])
    //println(customDeserializer)
    val block =
      if (!customDeserializer.isEmpty) {
        q"""{
          val $dserOps = ${ctx.prefix.tree}
          $customDeserializer.read($dserOps.deserializer)
        }"""
      }
      else {
        val tpe = weakTypeOf[O]
        val readers = extractReaders[D]

        val reader = findReader(readers, tpe)
        reader.map { readerMethod =>
          q"""{
            val $dserOps = ${ctx.prefix.tree}
            ${makeReaderWriterCall(q"$dserOps.deserializer", readerMethod)}
          }"""
        } getOrElse {
          val companionType = tpe.typeSymbol.companion.typeSignature
          companionType.decl(TermName("unapply")) match {
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
                unbindObject[D,O](partial, originalValue)
              }
            case _ =>
              unbindObject[D,O](partial, originalValue)
          }
        }
      }
    //println(block)
    block
  }

  def unbindOption[D: ctx.WeakTypeTag, O: ctx.WeakTypeTag]: ctx.Tree = {
    val dserOps = freshTerm("dserOps")
    val tpe = weakTypeOf[O]
    val elTpe = extractTypeArgs(tpe).head

    val block = q"""{
      val $dserOps = ${ctx.prefix.tree}
      if ($dserOps.deserializer.isNull)
        None
      else
        Some($dserOps.deserializer.unbind[$elTpe])
    }"""
    //println(block)
    block
  }

  def unbindEither[D: ctx.WeakTypeTag, O: ctx.WeakTypeTag]: ctx.Tree = {
    val dserOps = freshTerm("dserOps")
    val v = freshTerm("v")
    val rightIsBetter = freshTerm("rightIsBetter")
    val r = freshTerm("r")
    val r1 = freshTerm("r1")
    val r2 = freshTerm("r2")
    val e1 = freshTerm("e1")
    val e2 = freshTerm("e2")
    val tpe = weakTypeOf[O]
    val left = extractTypeArgs(tpe).head
    val right = extractTypeArgs(tpe).tail.head

    val leftDStr = getTypeValueString(left.tpe)
    val rightDStr = getTypeValueString(right.tpe)

    val block = q"""{
      val $dserOps = ${ctx.prefix.tree}
      import com.hypertino.binders.value._
      import scala.util._
      val $v = $dserOps.deserializer.unbind[com.hypertino.binders.value.Value]
      val $rightIsBetter = com.hypertino.binders.internal.Helpers.getConformity($leftDStr,$v) <=
        com.hypertino.binders.internal.Helpers.getConformity($rightDStr,$v)

      val $r = Try (if ($rightIsBetter) Right($v.to[$right]) else Left($v.to[$left]))
        match {
          case Success($r1) => $r1
          case Failure($e1) =>
            Try (if ($rightIsBetter) Left($v.to[$left]) else Right($v.to[$right]))
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

  def getTypeValueString(ct: Type) = {
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

  def unbindIterable[D: ctx.WeakTypeTag, O: ctx.WeakTypeTag]: ctx.Tree = {
    val dserOps = freshTerm("dserOps")
    val tpe = weakTypeOf[O]
    val elTpe = extractTypeArgs(tpe).head
    val q = q"""{
      val $dserOps = ${ctx.prefix.tree}
      ${convertIterator(tpe, q"$dserOps.deserializer.iterator().map(_.unbind[$elTpe])")}
    }"""
    //println(q)
    q
  }

  def unbindObject[D: ctx.WeakTypeTag, O: ctx.WeakTypeTag](partial: Boolean, originalValue: ctx.Tree): ctx.Tree = {
    val dserOps = freshTerm("dserOps")
    val i = freshTerm("i")
    val orig = freshTerm("orig")
    val extra = freshTerm("extra")
    val converter = createConverter[D]
    val caseClassParams = extractCaseClassParams[O]
    val companionSymbol = weakTypeOf[O].typeSymbol.companion
    val isExtraType = isWithExtra[O]

    val vars = caseClassParams.zipWithIndex.map { case (parameter, index) =>
      val varName = TermName("i_" + parameter.name.decodedName.toString)
      val fieldName = identToFieldName(parameter, converter)

      (
        // _1
        if (partial)
          q"var $varName : Option[${parameter.typeSignature}] = Some($orig.${parameter.name.toTermName})"
        else
          q"var $varName : Option[${parameter.typeSignature}] = None",

        // _2
        if (parameter.asTerm.isParamWithDefault) {
          Some(cq"""$fieldName => {
            $varName = $i.unbind[Option[${parameter.typeSignature}]]
          }""")
        } else {
          Some(cq"""$fieldName => {
            $varName = Some($i.unbind[${parameter.typeSignature}])
          }""")
        },

        // todo: do something with this! canBuildFrom + something 11
        // _3
        if (parameter.asTerm.isParamWithDefault) {
          val defVal = TermName("apply$default$" + (index + 1))
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
        else if (parameter.typeSignature <:< typeOf[TraversableOnce[_]])
          q"$parameter = $varName.getOrElse(${emptyTraversable(parameter.typeSignature)})"
        else
          q"$parameter = $varName.getOrElse(throw new com.hypertino.binders.core.FieldNotFoundException($fieldName))"
      )
    } ++ {
      if (isExtraType) {
        List(
          (
            q"var $extra: scala.collection.mutable.LinkedHashMap[String,com.hypertino.binders.value.Value] = null",
            None,
            q"{if ($extra == null) com.hypertino.binders.value.Obj.empty else com.hypertino.binders.value.Obj($extra)}"
          )
        )
      } else {
        List.empty
      }
    }

    val block = q"""{
      val $dserOps = ${ctx.prefix.tree}
      ${if (partial) { q"val $orig = $originalValue" } else q""}
      ..${vars.map(_._1)}
      $dserOps.deserializer.iterator().foreach{case $i =>
        $i.fieldName.map {
          case ..${vars.flatMap(_._2)}
          case other => {
            ${
              if (!isExtraType) {
                callIfExists[D](q"$i", "consume")
              } else {
                val convertedName = findConverter[D].map { converter =>
                    q"""
                      ${converter.termSymbol}.backwardConverter.map(_.convert($i.fieldName.get)).getOrElse($i.fieldName.get)
                    """
                  } getOrElse {
                    q"""
                      $i.fieldName.get
                    """
                  }
                q"""
                  if ($extra == null) {
                    $extra = new scala.collection.mutable.LinkedHashMap[String,com.hypertino.binders.value.Value]()
                  }
                  $extra += $convertedName -> $i.unbind[com.hypertino.binders.value.Value]
                """
              }
            }
          }
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

  def unbindMap[O: ctx.WeakTypeTag]: ctx.Tree = {
    val dserOps = freshTerm("dserOps")
    val el = freshTerm("el")
    val tpe = weakTypeOf[O]
    val elTpe = extractTypeArgs(tpe).tail.head
    val block = q"""{
      val $dserOps = ${ctx.prefix.tree}
      $dserOps.deserializer.iterator().map{ case $el =>
        ($el.fieldName.get, $el.unbind[$elTpe])
      }.toMap
    }"""
    //println(block)
    block
  }

  protected def emptyTraversable(ct: Type): Tree = {
    val elType = extractTypeArgs(ct).head
    q"implicitly[scala.collection.generic.CanBuildFrom[_,$elType,$ct]].apply().result()"
  }

  protected def convertIterator(ct: Type, iteratorTree: Tree): Tree = {
    val elType = extractTypeArgs(ct).head
    q"implicitly[scala.collection.generic.CanBuildFrom[_,$elType,$ct]].apply().++=($iteratorTree).result()"
  }

  protected def extractTypeArgs(tpe: Type): List[TypeTree] = {
    tpe.dealias match {
      case TypeRef(_, _, args) => args.map(TypeTree(_))
      case _ =>
        ctx.abort(ctx.enclosingPosition, s"Can't extract typeArgs from $tpe")
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
            ctx.abort(ctx.enclosingPosition, "Can't find generic arg source for " + select + " / " + genericTypeSymbol)
          }
        })
    }

  }

  protected def makeReaderWriterCall(elemTerm: Tree, method: (MethodSymbol, Map[Symbol, Type]), arguments: List[Tree] = List()): Tree = {
    val inner = applyTypeArgs(Select(elemTerm, method._1),  method._2,  method._1.typeParams)
    if (arguments.nonEmpty) {
      Apply(inner, arguments)
    }
    else {
      inner
    }
  }

  protected def mostMatching(methods: List[MethodSymbol], scoreFun: MethodSymbol => Option[(Int, Map[Symbol, Type])]): Option[(MethodSymbol, Map[Symbol, Type])] = {
    var rMax: Int = 0
    var mRes: Option[(MethodSymbol,Map[Symbol, Type])] = None
    methods.foreach({ m => // todo: replace to .max and remove vars
      val score = scoreFun(m) match {
        case Some((r, typeArgs)) =>
          if (r > rMax) {
            rMax = r
            mRes = Some(m, typeArgs)
          }
        case None => // do nothing
      }
      // println(s"Comparing $m score = $score")
      //println("Comparing " + m + " with arg type " + methodParSym.typeSignature + " for parameter " + parSym + " with type " + parSymType + " RES = " + r + " --- " + math.random)
    })
    mRes
  }

  protected def findWriter(writers: List[MethodSymbol], fieldType: Type /*, print: Boolean = false*/): Option[(MethodSymbol, Map[Symbol, Type])] = {
    //println (s"Looking writers for $fieldType from: $writers")
    mostMatching(writers, m => {
      val writerType = m.paramLists.head.head // parSym 0 - value
      //println(s"comparing ${writerType.typeSignature} with $fieldType method: $m")
      Some(compareTypes(writerType.typeSignature, fieldType/*, print*/, m.typeParams))
    })
  }


  def typeBoundsComply(withType: Type, genericType: Type, genericSymbol: Symbol, methodTypeParams: List[Symbol]): Boolean = {
    methodTypeParams.find(_ == genericSymbol).forall { typeParamSymbol ⇒
      typeParamSymbol.typeSignature match {
        case TypeBounds(lo, hi) ⇒
          lo <:< withType && withType <:< hi

        case other: Type ⇒
          withType <:< other
      }
    }
  }

  def compareGenericTypesBounds(dst: Type, src: Type, typeParams: List[Symbol], rl: Int): (Int, Map[Symbol, Type]) = {
    src match {
      case TypeRef(srcTpe, srcSym, _) if srcTpe == NoPrefix =>
        if (typeBoundsComply(dst, srcTpe, srcSym, typeParams))
          (20, Map(srcSym → dst))
        else
          (0, Map.empty)

      case _ ⇒ dst match {
        case TypeRef(dstTpe, dstSym, _) if dstTpe == NoPrefix =>
          if (typeBoundsComply(src, dstTpe, dstSym, typeParams))
            (20, Map(dstSym → src))
          else
            (0, Map.empty)

        case _ ⇒
          (rl, Map.empty)
      }
    }
  }

  def compareGenericWithArgs(dstTypeArgs: List[Type], srcTypeArgs: List[Type], typeParams: List[Symbol], rl: Int) : (Int, Map[Symbol, Type])= {
    val typeMap = collection.mutable.Map[Symbol, Type]()
    var r = rl
    // now check generic type args
    if (srcTypeArgs.size == dstTypeArgs.size) {
      for (i <- srcTypeArgs.indices) {
        val dstT = dstTypeArgs(i)
        val srcT = srcTypeArgs(i)
        val tR = compareTypes(dstT, srcT, typeParams)
        r = tR._1
        typeMap ++= tR._2
      }
    }
    else {
      r = 5
    }
    (r, typeMap.toMap)
  }

  def compareGenericNoPrefix(dstTpe: Type, dstSym: Symbol, src: Type, typeParams: List[Symbol], rl: Int): (Int, Map[Symbol, Type]) = {
    if (dstTpe == NoPrefix && typeBoundsComply(src, dstTpe, dstSym, typeParams)) {
      val typeMap = collection.mutable.Map[Symbol, Type]()
      typeMap += dstSym -> src
      (rl, typeMap.toMap)
    }
    else {
      (0, Map.empty)
    }
  }

  protected def compareTypes(dst: Type, src: Type, typeParams: List[Symbol]): (Int, Map[Symbol, Type]) = {
    if ((src =:= dst) && typeParams.isEmpty)
      (100, Map.empty)
    else if ((src <:< dst) && typeParams.isEmpty)
      (90, Map.empty)
    else if ((src weak_<:< dst) && typeParams.isEmpty)
      (80, Map.empty)
    else if (typeParams.nonEmpty) {
      (src, dst) match {
        case (TypeRef(srcTpe, srcSym, srcTypeArgs), TypeRef(dstTpe, dstSym, dstTypeArgs)) =>
          if (srcTpe != NoPrefix && src.typeSymbol.typeSignature =:= dst.typeSymbol.typeSignature) // Outer type is matched fully
            compareGenericWithArgs(dstTypeArgs, srcTypeArgs, typeParams, 50)
          else if (srcTpe != NoPrefix && src.baseClasses.exists(_.typeSignature =:= dst.typeSymbol.typeSignature)) // Outer type inherits
            compareGenericWithArgs(dstTypeArgs, srcTypeArgs, typeParams, 30)
          else {
            val rrl = compareGenericNoPrefix(dstTpe, dstSym, src, typeParams, 20)
            if (rrl._1 == 0) compareGenericNoPrefix(srcTpe, srcSym, dst, typeParams, 10) else rrl
          }

        case (TypeRef(srcTpe, srcSym, srcTypeArgs), _) =>
          compareGenericNoPrefix(srcTpe, srcSym, dst, typeParams, 10)


        case (_, TypeRef(dstTpe, dstSym, dstTypeArgs)) =>
          compareGenericNoPrefix(dstTpe, dstSym, src, typeParams, 20)

        case other ⇒
          (0, Map.empty)
      }
    } else {
      (0, Map.empty)
    }
  }

  protected def callIfExists[S: ctx.WeakTypeTag](o: ctx.Tree, methodName: String): ctx.Tree = {
    weakTypeOf[S].members.filter(member => member.isMethod &&
      member.name.decodedName.toString == methodName &&
      member.isPublic && {
      val m = member.asInstanceOf[MethodSymbol]
      //println("method: " + member.name.decoded + " params: " + m.paramss)
      m.paramLists.isEmpty ||
        (m.paramLists.size == 1 && allImplicits(List(m.paramLists.head))) ||
        (m.paramLists.size == 2 && m.paramLists.head.isEmpty && allImplicits(m.paramLists.tail))
    }
    ).map(_.asInstanceOf[MethodSymbol]).headOption.map { m =>
      q"$o.${TermName(methodName)}()"
    } getOrElse {
      q"{}"
    }
  }

  protected def extractWriters[T: ctx.WeakTypeTag]: List[MethodSymbol] = {
    weakTypeOf[T].members.filter(member => member.isMethod &&
      member.name.decodedName.toString.startsWith("write") &&
      member.isPublic && {
        val m = member.asInstanceOf[MethodSymbol]
        m.paramLists.nonEmpty &&
          (m.paramLists.tail.isEmpty || allImplicits(m.paramLists.tail)) &&
          m.paramLists.head.size == 1 // only 1 parameter
      }
    ).map(_.asInstanceOf[MethodSymbol]).toList
  }

  protected def findReader(readers: List[MethodSymbol], fieldType: Type): Option[(MethodSymbol, Map[Symbol, Type])] = {
    //println (s"Looking readers for $fieldType from: $readers")
    mostMatching(readers, m => {
      //println(s"comparing $fieldType with ${m.returnType} method: $m")
      Some(compareTypes(fieldType, m.returnType, m.typeParams))
    })
  }

  protected def extractReaders[T: ctx.WeakTypeTag]: List[MethodSymbol] = {
    weakTypeOf[T].members.filter(member => member.isMethod &&
      member.name.decodedName.toString.startsWith("read") &&
      member.isPublic && {
        val m = member.asInstanceOf[MethodSymbol]
        //println("method: " + member.name.decoded + " params: " + m.paramss)
        m.paramLists.isEmpty ||
          (m.paramLists.size == 1 && allImplicits(List(m.paramLists.head))) ||
          (m.paramLists.size == 2 && m.paramLists.head.isEmpty && allImplicits(m.paramLists.tail))
      }
    ).map(_.asInstanceOf[MethodSymbol]).toList
  }

  protected def allImplicits(symbols: List[List[Symbol]]): Boolean = symbols.flatten.forall(_.isImplicit)

  protected def isWithExtra[T: ctx.WeakTypeTag]: Boolean = {
    val extraType = typeOf[WithExtra]
    val dealiased = weakTypeOf[T].dealias
    dealiased <:< extraType
  }

  protected def extractCaseClassParams[T: ctx.WeakTypeTag]: List[ctx.Symbol] = {
    val dealiased = weakTypeOf[T].dealias
    val isExtraType = isWithExtra[T]
    val companioned = dealiased.typeSymbol
    val companionSymbol = companioned.companion
    val companionType = companionSymbol.typeSignature

    val constructorSymbol = dealiased.decl(termNames.CONSTRUCTOR)
    val args = if (constructorSymbol != NoSymbol) {
      val defaultConstructor =
        if (constructorSymbol.isMethod) constructorSymbol.asMethod
        else {
          val ctors = constructorSymbol.asTerm.alternatives
          ctors.map(_.asMethod).find(_.isPrimaryConstructor).get
        }
      defaultConstructor.paramLists.headOption.getOrElse(List.empty)
    } else {
      companionType.decl(TermName("unapply")) match {
        case NoSymbol =>
          List.empty
        case s =>
          val unapply = s.asMethod
          val unapplyReturnTypes = unapply.returnType match {
            case TypeRef(_, _, Nil) =>
              ctx.abort(ctx.enclosingPosition, s"Apply of $companionSymbol has no parameters. Are you using an empty case class?")
            case TypeRef(_, _, args) =>
              args.head match {
                case t@TypeRef(_, _, Nil) => Some(List(t))
                case t@TypeRef(_, _, arguments) =>
                  if (t <:< typeOf[Option[_]]) Some(List(t))
                  else if (t <:< typeOf[Seq[_]]) Some(List(t))
                  else if (t <:< typeOf[Set[_]]) Some(List(t))
                  else if (t <:< typeOf[Array[_]]) Some(List(t))
                  else if (t <:< typeOf[Map[_, _]]) Some(List(t))
                  else if (t <:< typeOf[Product]) Some(arguments)
                case _ => None
              }
            case _ => None
          }

          companionType.decl(TermName("apply")) match {
            case NoSymbol => ctx.abort(ctx.enclosingPosition, "No apply function found")
            case sym =>
              // searches apply method corresponding to unapply
              val applies = sym.asTerm.alternatives
              val apply = applies.collectFirst {
                case (apply: MethodSymbol) if apply.paramLists.headOption.map(_.map(_.asTerm.typeSignature)) == unapplyReturnTypes => apply
              } getOrElse {
                sym.asMethod
              }

              if (apply.paramLists.tail.nonEmpty)
                ctx.abort(ctx.enclosingPosition, "Couldn't use apply method with more than a single parameter group")

              val applyOrConstructor = dealiased.members.filter(_.isConstructor).collectFirst {
                case (cntr: MethodSymbol) if cntr.paramLists.headOption.map(_.map(_.asTerm.typeSignature)) == unapplyReturnTypes => cntr
              } getOrElse {
                apply
              }
              // println("apply found:" + apply)
              applyOrConstructor.paramLists.head
          }
      }
    }
    if (args.isEmpty) {
      ctx.abort(ctx.enclosingPosition, s"No setter or unapply function found for ${companioned.fullName}")
    }
//    println(args.map(_.name.))
    args.filterNot(_.name.toString == "extra" && isExtraType)
  }

  protected def identToFieldName(symbol: ctx.Symbol, converter: Option[Converter]): Literal = {
    val annotation = symbol.annotations.find(a => a.treeTpe == typeOf[com.hypertino.binders.annotations.fieldName])
    val (fieldName,useConverter) = annotation.map { a =>
      (a.arguments.head match {
        case Literal(Constant(s:String)) => s
        case _ => symbol.name.decodedName.toString
      },
        a.arguments.tail.head match {
        case Literal(Constant(b:Boolean)) => b
        case _ => false
      })
    } getOrElse {
      (symbol.name.decodedName.toString, true)
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

  protected def findConverter[T: ctx.WeakTypeTag]: Option[Type] = {
    val tpe = weakTypeOf[T]
    val converterTypeName = TypeName("nameConverterType")

    tpe.baseClasses.flatMap {
      baseSymbol =>
        val baseType = tpe.baseType(baseSymbol)
        val ct = baseType.decl(converterTypeName)
        ct match {
          case NoSymbol => None
          case _ =>
            val t = ct.typeSignature.asSeenFrom(tpe, baseSymbol)
            t.baseClasses.find(t.typeSymbol.isClass && _ == typeOf[Converter].typeSymbol).map { x =>
              t
            } orElse {
              ctx.abort(ctx.enclosingPosition, s"$tpe.nameConverterType: $t is not a valid Converter, please use PlainConverter if you don't need convert identifier names")
            }
        }
    }.headOption
  }

  protected def createConverter[T: ctx.WeakTypeTag]: Option[Converter] = {
    findConverter[T] map { t =>
      // this is synchronized because of bug in scala
      // http://docs.scala-lang.org/overviews/reflection/thread-safety.html
      this.synchronized {
        Runtime.createInstance(t.typeSymbol.fullName)
      }
    }
  }
}


