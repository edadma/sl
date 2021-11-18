package io.github.edadma.sl

trait SLValue {
  val clas: SLClass
}

case class SLNumber(n: Number) extends SLValue { val clas: SLClass = PrimitiveClass.NumberClass }
case class SLString(s: String) extends SLValue { val clas: SLClass = PrimitiveClass.StringClass }
