package io.github.edadma.sl

case class PrimitiveClass(name: String, supers: Seq[SLClass]) extends SLClass {
  SLClass.set += this
}

object PrimitiveClass {
  val AnyClass: PrimitiveClass = PrimitiveClass("Any", Nil)
  val NumberClass: PrimitiveClass = PrimitiveClass("Number", Seq(AnyClass))
  val BooleanClass: PrimitiveClass = PrimitiveClass("Boolean", Seq(AnyClass))
  val StringClass: PrimitiveClass = PrimitiveClass("String", Seq(AnyClass))
}
