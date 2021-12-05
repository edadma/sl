package io.github.edadma.fastparse

import fastparse._
import pprint._

import scala.annotation.{switch, tailrec}

object Main extends App {

  implicit val whitespace = { implicit ctx: ParsingRun[_] =>
    val input = ctx.input
    val startIndex = ctx.index

    @tailrec def rec(current: Int, state: Int): ParsingRun[Unit] = {
      if (!input.isReachable(current)) {
        if (state == 0 || state == 1) ctx.freshSuccessUnit(current)
        else if (state == 2) ctx.freshSuccessUnit(current - 1)
        else {
          ctx.cut = true
          val res = ctx.freshFailure(current)
          if (ctx.verboseFailures) ctx.setMsg(startIndex, () => Util.literalize("*/"))
          res
        }
      } else {
        val currentChar = input(current)

        (state: @switch) match {
          case 0 =>
            (currentChar: @switch) match {
              case ' ' | '\t' => rec(current + 1, state)
              case '/'        => rec(current + 1, state = 2)
              case _          => ctx.freshSuccessUnit(current)
            }
          case 1 =>
            if (currentChar == '\n')
              ctx.freshSuccessUnit(current)
            else
              rec(current + 1, 1)
          case 2 =>
            (currentChar: @switch) match {
              case '/' => rec(current + 1, state = 1)
              case '*' => rec(current + 1, state = 3)
              case _   => ctx.freshSuccessUnit(current - 1)
            }
          case 3 => rec(current + 1, state = if (currentChar == '*') 4 else state)
          case 4 =>
            (currentChar: @switch) match {
              case '/' => rec(current + 1, state = 0)
              case '*' => rec(current + 1, state = 4)
              case _   => rec(current + 1, state = 3)
            }
        }
      }
    }

    rec(current = ctx.index, state = 0)
  }

  def nl[_: P]: P[Unit] = P("\n")

  def module[_: P]: P[ModuleAST] =
    P(Start ~ statement.?.rep(sep = nl).map(_.filter(_.isDefined).map(_.get)).map(ModuleAST) ~ End)

  def statement[_: P]: P[StatAST] = new Parser(indent = 0).statement

  class Parser(indent: Int) {
    def statement[_: P]: P[StatAST] =
      P(
        "var" ~ (ident ~ ("=" ~ expressionOrBlock).?).map(VarStat.tupled) |
          expression.map(ExpressionStat)
      )

    def k[_: P](s: String): P[Unit] = P(s ~~ !CharPred(_.isLetterOrDigit))

    def deeper[_: P]: P[Int] = P(" ".repX(indent + 1).!.map(_.length)).log

    def block[_: P]: P[Seq[StatAST]] =
      P(
        "\n" ~~ deeper
          .flatMapX(i => new Parser(indent = i).statement.?.rep(1, sep = ("\n" + " " * i)./))
          .map(_.filter(_.isDefined).map(_.get)))

    def blockExpression[_: P]: P[ExprAST] = P(block map BlockExpr)

    def expressionOrBlock[_: P]: P[ExprAST] = P(expression | blockExpression)

    def keyword[_: P]: P[Unit] =
      StringIn(
        "div",
        "and",
        "or",
        "not",
        "break",
        "continue",
        "var",
        "val",
        "def",
        "mod",
        "if",
        "then",
        "true",
        "false",
        "null",
        "else",
        "elsif",
        "with",
        "extends",
        "class",
        "module",
        "match",
        "case",
        "for",
        "do",
        "while"
      ) ~ (!CharPred(_.isLetterOrDigit) | End)

    def ident[_: P]: P[Ident] =
      P((!keyword ~ Index ~ (CharIn("a-zA-Z_") ~~ CharIn("a-zA-Z0-9_").repX).!).map(Ident.tupled))

    def leftInfix(tree: (Int, ExprAST, Seq[(String, Int, ExprAST)])): ExprAST = {
      val (lpos, base, ops) = tree

      val (_, res) =
        ops.foldLeft((lpos, base)) {
          case ((lp, left), (op, rp, right)) => (lp, InfixExpr(lp, left, op, rp, right))
        }

      res
    }

    def number[_: P]: P[ExprAST] = P(CharIn("0-9").repX(1).!.map(IntegerExpr))

    def parens[_: P]: P[ExprAST] = P("(" ~/ comparitive ~ ")")

    def variable[_: P]: P[ExprAST] = P(ident map SymExpr)

    def factor[_: P]: P[ExprAST] = P(variable | number | parens)

    def comparitive[_: P]: P[ExprAST] =
      P(
        (Index ~ NoCut(additive) ~ (StringIn("<=", ">=", "!=", "<", ">", "==", "div").! ~/ Index ~ additive)
          .map(Predicate.tupled)
          .rep(1))
          .map(CompareExpr.tupled) | additive
      )

    def multiplicative[_: P]: P[ExprAST] =
      P(Index ~ factor ~ (StringIn("*", "/").! ~/ Index ~ factor).rep).map(leftInfix)

    def additive[_: P]: P[ExprAST] =
      P(Index ~ multiplicative ~ (StringIn("+", "-").! ~/ Index ~ multiplicative).rep).map(leftInfix)

    def expression[_: P]: P[ExprAST] = P(comparitive)
  }

  val input = {
    """
      |// asdf
      |var x =
      | 3+4 // asdf
      | var y =
      |  A
      |  // asdf
      |  B
      | 5+6
      |
      |123 //asdf
      |
      |// asdf
      |""".stripMargin
  }

  parse(input, module(_)) match {
    case Parsed.Success(value, index) => pprintln(value)
    case f: Parsed.Failure =>
      println(f)
      println(f.extra.trace())
  }

}
