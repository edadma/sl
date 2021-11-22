package io.github.edadma.sl

import scala.collection.immutable.ArraySeq

class Code(insts: collection.Seq[Inst]) {
  private val code = insts to ArraySeq

  def apply(idx: Int): Inst = code(idx)

  def length: Int = code.length

  override def toString: String = {
    code.zipWithIndex map { case (s, i) => f"$i% 4d $s" } mkString "\n"
  }
}
