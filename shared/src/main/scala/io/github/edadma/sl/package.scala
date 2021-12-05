package io.github.edadma

package object sl {

  def problem(pos: Int, msg: String): Nothing = {
    sys.error(msg)
  }

}
