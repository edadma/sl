package io.github.edadma.sl

case class PrimitiveClass(name: String, supers: Seq[SLClass]) extends SLClass {
  SLClass.set += this
}

object PrimitiveClass {
  val AnyClass: PrimitiveClass = PrimitiveClass("Any", Nil)
  val UnitClass: PrimitiveClass = PrimitiveClass("Unit", Seq(AnyClass))
  val NothingClass: PrimitiveClass = PrimitiveClass("Nothing", Seq(AnyClass))
  val UndefinedClass: PrimitiveClass = PrimitiveClass("Undefined", Seq(AnyClass))
  val NullClass: PrimitiveClass = PrimitiveClass("Null", Seq(AnyClass))
  val NumberClass: PrimitiveClass = PrimitiveClass("Number", Seq(AnyClass))
  val BooleanClass: PrimitiveClass = PrimitiveClass("Boolean", Seq(AnyClass))
  val IteratorClass: PrimitiveClass = PrimitiveClass("Iterator", Seq(AnyClass))
  val ListClass: PrimitiveClass = PrimitiveClass("List", Seq(AnyClass))
  val MapClass: PrimitiveClass = PrimitiveClass("Map", Seq(AnyClass))
  val StringClass: PrimitiveClass = PrimitiveClass("String", Seq(AnyClass))
  val StringBuilderClass: PrimitiveClass = PrimitiveClass("StringBuilder", Seq(AnyClass))
  val RangeClass: PrimitiveClass = PrimitiveClass("Range", Seq(AnyClass))
  val FunctionClass: PrimitiveClass = PrimitiveClass("Function", Seq(AnyClass))
  val ClassClass: PrimitiveClass = PrimitiveClass("Class", Seq(AnyClass))
}
