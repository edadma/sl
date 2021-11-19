package io.github.edadma.sl

abstract class Env {

  var _pos: Option[SLParser#Position] = None

  def pos(p: SLParser#Position): Unit = _pos = Some(p)

  def push(v: SLValue): Unit

  def pushn(n: Number): Unit = push(SLNumber(n))

  def pop: SLValue

  def popn: Number =
    pop match {
      case SLNumber(n) => n
      case x           => problem(_pos, s"number was expected, not '$x'")
    }

  def pops: String = pop.toString

  def symbol(name: String): Option[SLValue]

}
