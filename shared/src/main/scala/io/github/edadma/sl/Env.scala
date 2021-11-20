package io.github.edadma.sl

import scala.collection.mutable

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

class SimpleEnv extends Env {
  val stack = new mutable.Stack[SLValue]
  val vars = new mutable.HashMap[String, SLValue]

  override def push(v: SLValue): Unit = stack push v

  override def pop: SLValue = stack.pop()

  override def top: SLValue = stack.top

  override def symbol(name: String): Option[SLValue] = vars get name
}
