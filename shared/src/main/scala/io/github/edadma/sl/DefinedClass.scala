package io.github.edadma.sl

case class DefinedClass(name: String, supers: Seq[SLClass], constructor: Code, parms: Seq[String]) extends SLClass
