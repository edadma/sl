package io.github.edadma

package object sl {

  def problem(pos: Option[SLParser#Position], msg: String): Nothing = {
    sys.error(msg)
  }

}
