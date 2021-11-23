package io.github.edadma.sl

import pprint.pprintln

object Main extends App {
  val input =
    """
      |def add(a, b) = {
      | println('a', a)
      | println('b', b)
      |
      | def times_a(n) = n*a
      |
      | times_a(2) + b
      |}
      |
      |println(add(3, 4))
      |""".stripMargin
//    """
//      |def add(a, b) = {
//      | println('a', a)
//      | println('b', b)
//      |
//      | a + b
//      |}
//      |
//      |println(add(3, 4))
//      |""".stripMargin
//    """
//      |if 3 < 4 < 5 then println('yes')
//      |println('done')
//      |""".stripMargin
  val p = new SLParser(input)
  val t = p.parseSources

//  pprintln(t)

  val c = Compiler(t)

//  println(c)

  val e = new SourcesEnv(c)

//  e.trace = true
  e.run()

}
