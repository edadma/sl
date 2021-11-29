package io.github.edadma.sl

import scala.collection.immutable.ArraySeq

class Code(insts: collection.Seq[Inst]) extends SLValue {
  val clas: SLClass = PrimitiveClass.CodeClass

  private val code = insts to ArraySeq

  def apply(idx: Int): Inst = code(idx)

  def length: Int = code.length

  def listing(): Unit = {
    println(code.zipWithIndex map { case (s, i) => f"$i% 4d $s" } mkString "\n")
  }
}
