package io.github.edadma.sl

import scala.language.postfixOps

case class DefinedFunction(name: String, code: Code, parms: Seq[String]) extends SLValue with Callable {
  val clas: SLClass = PrimitiveClass.FunctionClass
  var outer: Activation = _

  def call(env: Env, args: Seq[SLValue]): Unit = {
    if (args.length != parms.length)
      env.problem(s"wrong number of arguments for '$name()': got ${args.length}, expected ${parms.length}")

    env.act = FunctionActivation(env.act, code, parms zip args toMap, outer)
  }

  override def execute(env: Env): Unit = {
    outer = env.act
    super.execute(env)
  }

  override def toString: String = s"[function: $name]"
}
