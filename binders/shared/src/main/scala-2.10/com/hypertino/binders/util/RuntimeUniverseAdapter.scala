package com.hypertino.binders.util

trait RuntimeUniverseAdapter {
  val universe = scala.reflect.runtime.universe
  import universe._

  val termNames = nme
  val typeNames = tpnme
}
