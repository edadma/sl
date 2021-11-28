package io.github.edadma.sl

case class Instance(con: ConstructorActivation) extends SLValue {
  val clas: DefinedClass = con.clas
}
