package com.hypertino.binders.internal

import com.hypertino.binders.core.FieldNotFoundException
import com.hypertino.binders.value.{Bool, Lst, Number, Obj, Text, Value, ValueVisitor}

object Helpers {
  def getFieldOrThrow[T](x: Option[T], fieldName: String): T = {
    if (x == null)
      throw new FieldNotFoundException(fieldName)
    x.getOrElse(throw new FieldNotFoundException(fieldName))
  }

  def getConformity(typ:String, value: Value): Int= {
    value ~~ new ValueVisitor[Int] {
      override def visitNumber(d: Number): Int = if (typ == "Number") 100 else 0
      override def visitBool(d: Bool): Int = if (typ == "Bool") 100 else 0
      override def visitObj(d: Obj): Int = if (typ == "Obj") 100 else 0
      override def visitText(d: Text): Int = if (typ == "Text") 100 else 0
      override def visitLst(d: Lst): Int = if (typ == "Lst") 100 else 0
      override def visitNull(): Int = 0
    }
  }
}
