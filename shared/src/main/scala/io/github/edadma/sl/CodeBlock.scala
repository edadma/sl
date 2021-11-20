package io.github.edadma.sl

import scala.collection.immutable.ArraySeq

class CodeBlock(insts: collection.Seq[Inst]) {
  private val code = insts to ArraySeq

  def apply(idx: Int): Inst = code(idx)

  def length: Int = code.length
}
