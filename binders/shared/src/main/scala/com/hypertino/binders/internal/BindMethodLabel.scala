package com.hypertino.binders.internal

trait BindMethodLabel[T] {
  def bindInside(t: T): Unit
}
