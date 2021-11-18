package io.github.edadma.sl

trait Inst

case class Push(v: SLValue) extends Inst
