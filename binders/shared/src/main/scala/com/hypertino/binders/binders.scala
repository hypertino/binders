package com.hypertino

import com.hypertino.binders.core.{Deserializer, Serializer}
import com.hypertino.binders.internal.BindersMacro

import scala.language.experimental.macros

package object binders {

  implicit class SerializerOps[S <: Serializer[_]](val serializer: S) extends AnyVal {
    def bind[O](value: O): S = macro BindersMacro.bind[S, O]

    def bindArgs(t: Any*): S = macro BindersMacro.bindArgs[S]

    def bindPartial[O](value: O): S = macro BindersMacro.bindPartial[S, O]
  }

  implicit class DeserializerOps[D <: Deserializer[_]](val deserializer: D) extends AnyVal {
    def unbind[O]: O = macro BindersMacro.unbind[D, O]

    def unbindPartial[O](originalValue: O): O = macro BindersMacro.unbindPartial[D, O]
  }
}

