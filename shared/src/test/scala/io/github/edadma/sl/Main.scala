package io.github.edadma.sl

import pprint.pprintln

object Main extends App {

  val input =
//    """
//      |x = 1
//      |
//      |while x <= 5 do {
//      | if 2 < x < 5 then println('3 or 4')
//      | println(x)
//      | x = x + 1
//      |}
//      |
//      |println('done')
//      |""".stripMargin
    """
      |if 3 < 3 < 5 then println('yes')
      |println('done')  
      |""".stripMargin
  val p = new SLParser(input)
  val t = p.parseSources

//  pprintln(t)

  val c = Compiler(t)

//  println(c)

  val e = new SimpleEnv(c)

//  e.trace = true
  e.run()

}
