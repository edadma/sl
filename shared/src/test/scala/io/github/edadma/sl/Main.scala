package io.github.edadma.sl

import pprint.pprintln

object Main extends App {

  val input =
    """
      |var x = 5
      |
      |var y
      |
      |def f(a, b) = {
      | g(a, b)
      | a + b
      |}
      |
      |f(3, 4)
      |""".stripMargin
  val p = new SLParser(input)
  val t = p.parseSources

  pprintln(t)

}
