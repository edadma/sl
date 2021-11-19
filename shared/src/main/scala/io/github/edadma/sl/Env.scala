package io.github.edadma.sl

abstract class Env {

  def push(v: SLValue): Unit

  def pushn(n: Number): Unit = push(SLNumber(n))

  def pop: SLValue

  def popn: Number

  def pops: String = pop.toString

  def pos(p: SLParser#Position): Unit

}
