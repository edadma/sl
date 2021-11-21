package io.github.edadma.sl

case class PrimitiveClass(name: String, supers: Seq[SLClass]) extends SLClass {
  SLClass.set += this
}

object PrimitiveClass {
  val AnyClass: PrimitiveClass = PrimitiveClass("Any", Nil)
  val UnitClass: PrimitiveClass = PrimitiveClass("Unit", Seq(AnyClass))
  val NullClass: PrimitiveClass = PrimitiveClass("Null", Seq(AnyClass))
  val NumberClass: PrimitiveClass = PrimitiveClass("Number", Seq(AnyClass))
  val BooleanClass: PrimitiveClass = PrimitiveClass("Boolean", Seq(AnyClass))
  val StringClass: PrimitiveClass = PrimitiveClass("String", Seq(AnyClass))
  val BuiltinClass: PrimitiveClass = PrimitiveClass("Builtin", Seq(AnyClass))
  val FunctionClass: PrimitiveClass = PrimitiveClass("Function", Seq(AnyClass))
}
