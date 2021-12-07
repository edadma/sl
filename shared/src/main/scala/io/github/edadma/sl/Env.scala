package io.github.edadma.sl

import scala.collection.mutable

abstract class Env {

  var _pos: Option[Int] = None
  var trace: Boolean = false

  val stack = new mutable.Stack[SLValue]
  var act: Activation

  def symbol(name: String): SLValue

  def lvalue(name: String): SLValue

  def run(): Unit =
    while (act.ip < act.code.length) {
      val inst = act.code(act.ip)

      if (trace)
        println(f"${act.ip}% 3d $inst")

      act.ip += 1
      inst execute this

      if (trace)
        println(s"    ${stack mkString ", "}")
    }

  def apply(n: Int): SLValue = stack(n)

  def push(v: SLValue): Unit = stack push v

  def branch(disp: Int): Unit = act.ip += disp

  def pop: SLValue = stack.pop()

  def top: SLValue = stack.top

  def pos(p: Int): Unit = _pos = Some(p)

  def pushn(n: Number): Unit = push(SLNumber(n))

  def pushb(b: Boolean): Unit = push(SLBoolean(b))

  def popn: Number =
    pop.deref match {
      case SLNumber(n) => n
      case x           => problem(s"number was expected, not '$x'")
    }

  def popb: Boolean =
    pop.deref match {
      case SLBoolean(b) => b
      case x            => problem(s"boolean was expected, not '$x'")
    }

  def topb: Boolean =
    top.deref match {
      case SLBoolean(b) => b
      case x            => problem(s"boolean was expected, not '$x'")
    }

  def popi: Int =
    pop.deref match {
      case SLInteger(n) => n
      case x            => problem(s"integer was expected, not '$x'")
    }

  def pops: String = pop.deref.toString

  def problem(msg: String): Nothing = {
    // todo: use _pos
    sys.error(s"${_pos.getOrElse("no pos")}: $msg")
  }

}

class ModuleEnv(code: Code) extends Env {

  var act: Activation = ModuleActivation(code)

  def symbol(name: String): SLValue = {
    act.symbol(name) getOrElse problem(s"symbol not found: $name")
  }

  def lvalue(name: String): SLValue = act.symbol(name) getOrElse act.define(name, new VarMutable(SLNull))

}
