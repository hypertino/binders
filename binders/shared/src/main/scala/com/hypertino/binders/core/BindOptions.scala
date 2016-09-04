package com.hypertino.binders.core

case class BindOptions(skipOptionalFields: Boolean)

object BindOptions {
  implicit val defaultBindOptions = new BindOptions(false)
  def get(implicit ops: BindOptions): BindOptions = ops
}
