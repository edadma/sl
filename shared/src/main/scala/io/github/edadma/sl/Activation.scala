package io.github.edadma.sl

case class Activation(caller: Activation, block: CodeBlock) extends SLValue {
  override def clas: SLClass = null

  var ip = 0
}
