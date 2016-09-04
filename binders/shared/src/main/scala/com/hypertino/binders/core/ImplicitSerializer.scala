package com.hypertino.binders.core

trait ImplicitSerializer[T, S <: Serializer[_]] {
  def write(serializer: S, value: T)
}
