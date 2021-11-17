package io.github.edadma.sl

import pprint.pprintln

object Main extends App {

  val input =
    """
      |a + b + c
      |""".stripMargin
  val p = new SLParser(input)
  val t = p.parseSources

  pprintln(t)

}
