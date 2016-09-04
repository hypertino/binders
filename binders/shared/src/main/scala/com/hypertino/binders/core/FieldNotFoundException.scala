package com.hypertino.binders.core

class FieldNotFoundException(val fieldName: String) extends BindersException(s"Field $fieldName wasn't found")
