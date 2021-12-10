package io.github.edadma.sl

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

import io.github.edadma.dal._

trait SLValue extends Inst {
  def clas: SLClass

  def deref: SLValue = this

  def execute(env: Env): Unit = env push this

  def render: String =
    this match {
      case SLString(s) => s"${'"'}$s${'"'}"
      case _           => this.toString
    }

  override def toString: String =
    this match {
      case SLMap(m)  => m.map { case (k, v) => s"${k.render}: ${v.render}" }.mkString("{", ", ", "}")
      case SLList(l) => l.map(_.render).mkString("[", ", ", "]")
    }
}

object SLValue {

  val EPSILON: SLString = SLString("")
  val EMPTY: SLMap = SLMap(Map())
  val FALSE: SLBoolean = SLBoolean(false)
  val TRUE: SLBoolean = SLBoolean(true)
  val ZERO: SLNumber = SLNumber(0)
  val ONE: SLNumber = SLNumber(1)
  val NIL: SLList = SLList(Nil)

}

case object SLVoid extends SLValue {
  val clas: SLClass = PrimitiveClass.UnitClass

  override def toString: String = "()"
}

case object SLUndefined extends SLValue {
  val clas: SLClass = PrimitiveClass.UndefinedClass

  override def toString: String = "[[undefined]]"
}

case object SLNull extends SLValue {
  val clas: SLClass = PrimitiveClass.NullClass

  override def toString: String = "null"
}

object SLNumber {
  def apply(n: Int): SLNumber = SLNumber(IntType, n)

  def apply(n: (Type, Number)): SLNumber = SLNumber(n._1, n._2)
}

case class SLNumber(typ: Type, value: Number) extends SLValue with TypedNumber {
  val clas: SLClass = PrimitiveClass.NumberClass

  override def toString: String = value.toString
}

case class SLList(l: List[SLValue]) extends SLValue {
  val clas: SLClass = PrimitiveClass.ListClass
}

case class SLMap(m: Map[SLValue, SLValue]) extends SLValue {
  val clas: SLClass = PrimitiveClass.MapClass
}

case class SLBoolean(b: Boolean) extends SLValue {
  val clas: SLClass = PrimitiveClass.BooleanClass

  override def toString: String = b.toString
}

case class SLString(s: String) extends SLValue {
  val clas: SLClass = PrimitiveClass.StringClass

  override def toString: String = s
}

case class SLStringBuilder(builder: StringBuilder = new StringBuilder) extends SLValue {
  val clas: SLClass = PrimitiveClass.StringBuilderClass

  override def toString: String = builder.toString
}

case class SLRange(start: Int, end: Int) extends SLValue {
  val clas: SLClass = PrimitiveClass.StringClass
  val range = start to end

  override def toString: String = s"$start..$end"
}

trait Callable {
  def call(env: Env, args: Seq[SLValue]): Unit
}

case class SLNativeFunction(name: String, f: PartialFunction[Seq[SLValue], SLValue]) extends SLValue with Callable {
  val clas: SLClass = PrimitiveClass.FunctionClass

  def call(env: Env, args: Seq[SLValue]): Unit =
    if (f.isDefinedAt(args))
      env push f(args)
    else
      env problem s"invalid arguments for function '$name'"

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
