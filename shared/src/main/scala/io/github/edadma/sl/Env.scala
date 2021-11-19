package io.github.edadma.sl

abstract class Env {

  var _pos: Option[SLParser#Position] = None

  def pos(p: SLParser#Position): Unit = _pos = Some(p)

  def push(v: SLValue): Unit

  def pushn(n: Number): Unit = push(SLNumber(n))

  def pop: SLValue

  def top: SLValue

  def popn: Number =
    pop match {
      case SLNumber(n) => n
      case x           => problem(s"number was expected, not '$x'")
    }

  def pops: String = pop.toString

  def symbol(name: String): Option[SLValue]

  def problem(msg: String): Nothing = {
    // todo: use _pos
    sys.error(msg)
  }

}
