package io.github.edadma.sl

trait SLValue

case class SLNumber(n: Number) extends SLValue
case class SLString(s: String) extends SLValue
