package io.github.edadma.sl

import scala.collection.mutable

abstract class Env {

  var _pos: Option[SLParser#Position] = None
  var trace: Boolean = false

  val stack = new mutable.Stack[SLValue]
  var act: Activation

  def run(): Unit =
    while (act.ip < act.block.length) {
      val inst = act.block(act.ip)

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

  def pos(p: SLParser#Position): Unit = _pos = Some(p)

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

  def symbol(name: String): SLValue

  def problem(msg: String): Nothing = {
    // todo: use _pos
    sys.error(msg)
  }

}

class SimpleEnv(block: CodeBlock) extends Env {

  var act: Activation = new Activation(null, block, Map())
  val vars: mutable.Map[String, SLValue] =
    mutable.HashMap[String, SLValue](
      "println" -> SLBuiltin("println", args => {
        println(args mkString ", ")
        SLVoid
      })
    )

  def symbol(name: String): SLValue = {
    act.vars get name match {
      case Some(value) => value
      case None =>
        vars get name match {
          case Some(value) => value
          case None =>
            val mut = new VarMutable(SLNull)

            vars(name) = mut
            mut
        }
    }
  }

}
