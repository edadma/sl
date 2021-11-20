package io.github.edadma.sl

import pprint.pprintln

object Main extends App {

  val input =
    """
      |x = 1
      |
      |while x do {
      | println(x)
      | x = x + 1
      |}
      |""".stripMargin
  val p = new SLParser(input)
  val t = p.parseSources

  pprintln(t)

  val c = Compiler(t)
  val e = new SimpleEnv(c)

  e.run()

}
