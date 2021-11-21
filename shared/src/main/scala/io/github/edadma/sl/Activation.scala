package io.github.edadma.sl

case class Activation(caller: Activation, block: CodeBlock, vars: Map[String, SLValue]) {
  var ip = 0
}
