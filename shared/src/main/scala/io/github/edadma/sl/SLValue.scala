package io.github.edadma.sl

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait SLValue {
  def clas: SLClass

  def deref: SLValue = this
}

case class SLNumber(n: Number) extends SLValue {
  val clas: SLClass = PrimitiveClass.NumberClass

  override def toString: String = n.toString
}

case class SLString(s: String) extends SLValue {
  val clas: SLClass = PrimitiveClass.StringClass

  override def toString: String = s
}

trait Applicable

case class SLFunction(name: String, f: Seq[SLValue] => SLValue) extends SLValue with Applicable {
  val clas: SLClass = PrimitiveClass.StringClass

  override def toString: String = s"[function: $name]"
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
