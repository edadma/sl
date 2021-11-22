package io.github.edadma.sl

import scala.collection.mutable

case class Activation(caller: Activation, block: Code, args: Map[String, SLValue]) {
  val locals = new mutable.HashMap[String, SLValue]
  var ip = 0
}
