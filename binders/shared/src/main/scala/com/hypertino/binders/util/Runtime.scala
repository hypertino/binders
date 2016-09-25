package com.hypertino.binders.util

import com.hypertino.inflector.naming.Converter

import scala.util.control.NonFatal

object Runtime extends RuntimeUniverseAdapter {
  import universe._

  def createInstance(className: String) = {
    val clz = Class.forName(className)
    val mirror = runtimeMirror(getClass.getClassLoader)

    try {
      val moduleSymbol = mirror.moduleSymbol(clz)
      val module = mirror.reflectModule(moduleSymbol)
      module.instance.asInstanceOf[Converter]
    }
    catch {
      case NonFatal(e) ⇒
        val sym = mirror.classSymbol(clz)
        val r = mirror.reflectClass(sym)
        val m = r.symbol.typeSignature.member(termNames.CONSTRUCTOR).asMethod
        val ctr = r.reflectConstructor(m)
        ctr().asInstanceOf[Converter]
    }
  }
}
