package io.github.edadma.sl

import scala.language.postfixOps

case class DefinedFunction(name: String, code: Code, parms: Seq[String], outer: Activation)
    extends SLValue
    with Callable {
  val clas: SLClass = PrimitiveClass.FunctionClass

  def call(env: Env, args: Seq[SLValue]): Unit = {
    if (args.length != parms.length)
      env.problem(s"wrong number of arguments for '$name()': got ${args.length}, expected ${parms.length}")

    env.act = FunctionActivation(env.act, code, parms zip args toMap, outer)
  }

  override def toString: String = s"[function: $name, $outer]"
}
