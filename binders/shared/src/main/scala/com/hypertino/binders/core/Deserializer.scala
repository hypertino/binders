package com.hypertino.binders.core

import com.hypertino.inflector.naming.Converter

trait Deserializer[C <: Converter] {
  type nameConverterType = C
  def fieldName: Option[String]
  def isNull: Boolean
}
