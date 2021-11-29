package io.github.edadma.sl

import scala.collection.mutable

trait Activation {
  val block: Code
  var ip: Int

  def define(name: String, value: SLValue): SLValue

  def symbol(name: String): Option[SLValue]

  def lvalue(name: String): SLValue
}

case class ModuleActivation(block: Code) extends Activation {
  val locals = new mutable.HashMap[String, SLValue]
  var ip = 0

  def define(name: String, value: SLValue): SLValue = {
    locals(name) = value
    value
  }

  def symbol(name: String): Option[SLValue] = locals get name

  def lvalue(name: String): SLValue = locals.getOrElse(name, define(name, new VarMutable(SLNull)))
}

trait FunctionLikeActivation extends Activation {
  val caller: Activation
}

case class FunctionActivation(caller: Activation, block: Code, args: Map[String, SLValue], outer: Activation)
    extends FunctionLikeActivation {
  val locals = new mutable.HashMap[String, SLValue]
  var ip = 0

  def define(name: String, value: SLValue): SLValue = {
    locals(name) = value
    value
  }

  def symbol(name: String): Option[SLValue] = args get name orElse (locals get name orElse outer.symbol(name))

  def lvalue(name: String): SLValue =
    args.getOrElse(name, locals.getOrElse(name, outer.symbol(name).getOrElse(define(name, new VarMutable(SLNull)))))
}

case class ConstructorActivation(clas: DefinedClass,
                                 caller: Activation,
                                 block: Code,
                                 args: Map[String, SLValue],
                                 outer: Activation)
    extends FunctionLikeActivation {
  val locals = new mutable.HashMap[String, SLValue]
  var ip = 0

  def define(name: String, value: SLValue): SLValue = {
    locals(name) = value
    value
  }

  def symbol(name: String): Option[SLValue] = args get name orElse (locals get name orElse outer.symbol(name))

  def lvalue(name: String): SLValue = {
    args.getOrElse(name, locals.getOrElse(name, outer.symbol(name).getOrElse(define(name, new VarMutable(SLNull)))))
  }
}
