package com.hypertino.binders.core

import com.hypertino.inflector.naming.Converter

trait Serializer[C <: Converter] {
  type nameConverterType = C
  def writeNull()
}
