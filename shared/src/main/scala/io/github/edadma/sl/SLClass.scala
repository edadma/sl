package io.github.edadma.sl

import scala.collection.mutable

trait SLClass {
  val name: String
  val supers: Seq[SLClass]
}

object SLClass {
  val set = new mutable.HashSet[SLClass]
}
