package io.github.edadma.sl

import scala.collection.mutable

trait SLClass {
  val name: String
  val supers: Seq[SLClass]
}

object SLClass {
  val set = new mutable.HashSet[SLClass]
}

case class PrimitiveClass(name: String, supers: Seq[SLClass]) extends SLClass {
  SLClass.set += this
}

object PrimitiveClass {
  val AnyClass: PrimitiveClass = PrimitiveClass("Number", Nil)
  val NumberClass: PrimitiveClass = PrimitiveClass("Number", Seq(AnyClass))
  val StringClass: PrimitiveClass = PrimitiveClass("String", Seq(AnyClass))
}
