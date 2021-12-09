package io.github.edadma.sl

import fastparse._
import pprint.pprintln

import scala.annotation.tailrec

object Main extends App {
  val input =
//    """
//      |def p(b)
//      | if b then
//      |  println('yes')
//      |  println('wow')
//      | else
//      |  println('no')
//      |  println('wee')
//      |
//      |p(true)
//      |p(false)
//      |println('done')
//      |""".stripMargin
//    """
//      |var count = 1
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
//      |println(x++)
//      |println(x)
//      |println(x, x++, x)
//      |""".stripMargin
    """
      |class c(a)
      | x = a + 10
      | var y
      |
      | def m = a + 20
      |
      | def n = y + 3
      |
      |o1 = c(3)
      |o2 = c(4)
      |
      |println(o1.x, o1.m(), o2.x, o2.m())
      |o1.y = 5
      |o2.y = 6
      |println(o1.y, o1.n(), o2.y, o2.n())
      |""".stripMargin
//    """
//      |x = 5
//      |
//      |def f(x) = x + 3
//      |
//      |println(f(4))
//      |
//      |println(`x = $x and f(4) = ${f(4)}.`)
//      |""".stripMargin
//    """
//      |var x = 1
//      |
//      |while true
//      | println(x)
//      | if x++ == 3 then break
//      |
//      |println('done')
//      |""".stripMargin
//    """
//      |var count = 1
//      |
//      |while count <= 5
//      | println(count++)
//      |
//      |println('done')
//      |""".stripMargin
//    """
//      |def add(a, b) =
//      | println('a', a)
//      | println('b', b)
//      |
//      | times_a = n -> n*a
//      |
//      | times_a(2) + b
//      |
//      |println(add(3, 4), add(5, 6))
//      |println((a -> 3*a)(4))
//      |""".stripMargin
//    """
//      |var count = 1
//      |
//      |while count <= 5
//      | println(count++)
//      | if count <= 3
//      |  continue
//      | println('loop')
//      |
//      |println('done')
//      |""".stripMargin
//    """
//        |var count = 1
//        |
//        |do
//        | println(count++)
//        | if count <= 3
//        |  continue
//        | println('loop', count)
//        |while count <= 5
//        |
//        |println('done')
//        |""".stripMargin
//    """
//      |println(123)
//      |""".stripMargin
  parse(input, SLParser.module(_)) match {
    case Parsed.Success(t, index) =>
//      pprintln(t)

      val c = Compilation(t)

//  c.listing()

      val e = new ModuleEnv(c)

//      e.trace = true
      e.run()
    case f: Parsed.Failure =>
      println(f.msg)
  }

}
