package io.github.edadma.sl

import scala.language.postfixOps

case class DefinedClass(name: String, supers: Seq[SLClass], code: Code, parms: Seq[String])
    extends SLClass
    with Callable {
  var outer: Activation = _

  def call(env: Env, args: Seq[SLValue]): Unit = {
    if (args.length != parms.length)
      env.problem(s"wrong number of arguments for '$name()': got ${args.length}, expected ${parms.length}")

    env.act = ConstructorActivation(this, env.act, code, parms zip args toMap, outer)
  }

  override def execute(env: Env): Unit = {
    outer = env.act
    super.execute(env)
  }

  override def toString: String = s"[constructor: $name]"
}
