package io.github.edadma.sl

import pprint.pprintln
import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec

object Main extends App {
  val input =
//    """
//      |def add(a, b) = {
//      | println('a', a)
//      | println('b', b)
//      |
//      | times_a = n -> n*a
//      |
//      | times_a(2) + b
//      |}
//      |
//      |println(add(3, 4))
//      |println(a -> 2*a)
//      |""".stripMargin
//    """
//      |def add(a, b) =
//      | println('a', a)
//      | println('b', b)
//      |
//      | a + b
//      |
//      |println(add(3, 4))
//      |""".stripMargin
//    """
//      |if true then
//      | println('yes')
//      | println('wow')
//      |else
//      | println('no')
//      | println('wee')
//      |
//      |println('done')
//      |""".stripMargin
//    """
//      |count = 1
//      |
//      |def f(x)
//      | println(x)
//      | x + 2
//      |
//      |y = f(5)
//      |
//      |println(y)
//      |
//      |while count <= 5
//      | println(count++)
//      |else
//      | println('else')
//      |
//      |println('done')
//      |""".stripMargin
//    """
//      |println(x = y = 123)
//      |println(x, y)
//      |println(++x)
//      |println(x)
//      |println(x, ++x, x)
//      |""".stripMargin
    """
      |class c(a)
      | def m1(x) = x + a
      |
      |println('123')
      |""".stripMargin

  val buf = new StringBuilder

  @tailrec
  def readch(r: CharReader): Unit =
    if (r.more) {
      buf +=
        (r.ch match {
          case c => c
        })
      readch(r.next)
    }

  readch(CharReader.fromString(input, indentation = Some(("//", "/*", "*/"))))

  val p = new SLParser(buf.toString)
  val t = p.parseSources

//  pprintln(t)

  val c = Compiler(t)

//  println(c)

  val e = new SourcesEnv(c)

//  e.trace = true
  e.run()

}
