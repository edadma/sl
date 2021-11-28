package io.github.edadma.sl

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

trait SLValue extends Inst {
  def clas: SLClass

  def deref: SLValue = this

  def execute(env: Env): Unit = env push this
}

object SLValue {

  val FALSE: SLBoolean = SLBoolean(false)
  val TRUE: SLBoolean = SLBoolean(true)
  val ZERO: SLNumber = SLNumber(0)
  val ONE: SLNumber = SLNumber(1)
  val NIL: SLList = SLList(Nil)

}

case class SLInteger(n: Int) extends SLValue {
  val clas: SLClass = PrimitiveClass.NumberClass

  override def toString: String = n.toString
}

case object SLVoid extends SLValue {
  val clas: SLClass = PrimitiveClass.UnitClass

  override def toString: String = "()"
}

case object SLNull extends SLValue {
  val clas: SLClass = PrimitiveClass.NullClass

  override def toString: String = "null"
}

case class SLNumber(n: Number) extends SLValue {
  val clas: SLClass = PrimitiveClass.NumberClass

  override def toString: String = {
    val d = n.doubleValue

    if (d.isWhole) n.longValue.toString else d.toString
  }
}

case class SLList(l: List[SLValue]) extends SLValue {
  val clas: SLClass = PrimitiveClass.ListClass

  override def toString: String = l.mkString("[", ",", "]")
}

case class SLBoolean(b: Boolean) extends SLValue {
  val clas: SLClass = PrimitiveClass.BooleanClass

  override def toString: String = b.toString
}

case class SLString(s: String) extends SLValue {
  val clas: SLClass = PrimitiveClass.StringClass

  override def toString: String = s
}

trait Callable {
  def call(env: Env, args: Seq[SLValue]): Unit
}

case class SLNativeFunction(name: String, f: Seq[SLValue] => SLValue) extends SLValue with Callable {
  val clas: SLClass = PrimitiveClass.FunctionClass

  def call(env: Env, args: Seq[SLValue]): Unit = env push f(args)

  override def toString: String = s"[built-in function: $name]"
}

abstract class Mutable extends SLValue {
  def value: SLValue

  def value_=(v: SLValue): Unit

  def clas: SLClass = value.clas

  override def deref: SLValue = value

  override def toString: String = value.toString
}

class VarMutable(var value: SLValue) extends Mutable

class MapMutable(map: mutable.Map[String, SLValue], key: String) extends Mutable {
  def value: SLValue = map(key)

  def value_=(v: SLValue): Unit = map(key) = v
}

class ArrayMutable(array: ArrayBuffer[SLValue], idx: Int) extends Mutable {
  def value: SLValue = array(idx)

  def value_=(v: SLValue): Unit = array(idx) = v
}
