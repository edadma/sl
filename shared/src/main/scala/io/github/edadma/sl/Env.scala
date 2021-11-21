package io.github.edadma.sl

import scala.collection.mutable

abstract class Env {

  var _pos: Option[SLParser#Position] = None
  var trace: Boolean = false

  def run(): Unit

  def branch(disp: Int): Unit

  def apply(n: Int): SLValue

  def push(v: SLValue): Unit

  def pop: SLValue

  def top: SLValue

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

  val stack = new mutable.Stack[SLValue]
  val vars: mutable.Map[String, SLValue] =
    mutable.HashMap[String, SLValue](
      "println" -> SLFunction("println", args => {
        println(args mkString ", ")
        SLVoid
      })
    )
  var ip = 0

  def run(): Unit =
    while (ip < block.length) {
      val inst = block(ip)

      if (trace)
        println(f"$ip% 3d $inst")

      ip += 1
      inst execute this

      if (trace)
        println(s"    ${stack mkString ", "}")
    }

  def apply(n: Int): SLValue = stack(n)

  def push(v: SLValue): Unit = stack push v

  def branch(disp: Int): Unit = ip += disp

  def pop: SLValue = stack.pop()

  def top: SLValue = stack.top

  def symbol(name: String): SLValue =
    vars get name match {
      case Some(value) => value
      case None =>
        val mut = new VarMutable(SLNull)

        vars(name) = mut
        mut
    }

}
