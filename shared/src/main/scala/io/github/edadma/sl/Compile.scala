package io.github.edadma.sl

import scala.collection.mutable.ArrayBuffer

object Compile {

  def apply(src: SourcesAST): CodeBlock = {
    val buf = new ArrayBuffer[Inst]

    new CodeBlock(buf)
  }

}
