package io.github.edadma.sl

import pprint.pprintln

object Main extends App {

  val input =
    """
      |x = 1
      |
      |while x <= 5 do {
      | println(x)
      | x = x + 1
      |}
      |
      |println('done')
      |""".stripMargin
//    """
//      |println(123)
//      |""".stripMargin
  val p = new SLParser(input)
  val t = p.parseSources

//  pprintln(t)

  val c = Compiler(t)
  val e = new SimpleEnv(c)

//  e.trace = true
  e.run()

}
