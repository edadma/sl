package io.github.edadma

package object sl {

  def problem(pos: Cursor, msg: String): Nothing = {
    sys.error(msg)
  }

}
