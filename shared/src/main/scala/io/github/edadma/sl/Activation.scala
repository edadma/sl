package io.github.edadma.sl

import scala.collection.mutable

trait Activation {
  val code: Code
  var ip: Int

  def define(name: String, value: SLValue): SLValue

  def symbol(name: String): Option[SLValue]
}

case class ModuleActivation(code: Code) extends Activation {
  val locals = new mutable.HashMap[String, SLValue]
  var ip = 0

  def define(name: String, value: SLValue): SLValue = {
    locals(name) = value
    value
  }

  def symbol(name: String): Option[SLValue] = locals get name orElse (Global.map get name)
}

trait FunctionLikeActivation extends Activation {
  val caller: Activation
}

case class FunctionActivation(caller: Activation, code: Code, args: Map[String, SLValue], outer: Activation)
    extends FunctionLikeActivation {
  val locals = new mutable.HashMap[String, SLValue]
  var ip = 0

  def define(name: String, value: SLValue): SLValue = {
    locals(name) = value
    value
  }

  def symbol(name: String): Option[SLValue] = args get name orElse (locals get name orElse outer.symbol(name))
}

case class ConstructorActivation(clas: DefinedClass,
                                 caller: Activation,
                                 code: Code,
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
}
