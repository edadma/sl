package io.github.edadma.sl

import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec

object Main extends App {
  val input =
//    """
//      |def add(a, b) =
//      | println('a', a)
//      | println('b', b)
//      |
//      | times_a = n -> n*a
//      |
//      | times_a(2) + b
//      |
//      |println(add(3, 4))
//      |println((a -> 3*a)(4))
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
      | def m = a + 1
      |
      |o1 = c(3)
      |o2 = c(4)
      |
      |println(o1.m(), o2.m())
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

  val c = Compilation(t)

//  c.listing()

  val e = new SourcesEnv(c)

//  e.trace = true
  e.run()

}
