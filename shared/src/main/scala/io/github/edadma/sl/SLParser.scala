package io.github.edadma.sl

import fastparse._
import pprint._

import scala.annotation.{switch, tailrec}

object SLParser {

  private val delimiters = "[](){}`'\","

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
          if (ctx.verboseFailures) ctx.setMsg(startIndex, () => Util.literalize("*/")) // todo: this seems to depdend on pprint
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
        kw("var") ~ (ident ~ ("=" ~ expressionOrBlock).?).map(VarStat.tupled) |
          kw("class") ~ (ident ~ parameters ~ block).map(ClassStat.tupled) |
          kw("def") ~ (ident ~ parameters ~ ("=" ~ expression | "=".? ~ blockExpression)).map(DefStat.tupled) |
          expression.map(ExpressionStat)
      )

    def parameters[_: P]: P[Seq[Ident]] = P(("(" ~ ident.rep(sep = ",") ~ ")").?.map(_.getOrElse(Nil)))

    def kw[_: P](s: String): P[Unit] = P(s ~~ !CharPred(_.isLetterOrDigit))

    def sym[_: P](s: String): P[Unit] = P(s ~~ !CharPred(c => !c.isLetter && !delimiters.contains(c)))

    def expression[_: P]: P[ExprAST] = P(function)

    def functionParameters[_: P]: P[Seq[Ident]] = P(ident.map(id => Seq(id)) | "(" ~ ident.rep(sep = ",") ~ ")")

    def function[_: P]: P[ExprAST] =
      P((functionParameters ~ "->" ~ Index ~ expressionOrBlock).map(FunctionExpr.tupled) | assignment)

    def assignment[_: P]: P[ExprAST] =
      P((Index ~ applicative ~ "=" ~ Index ~ expressionOrBlock).map(AssignExpr.tupled) | construct)

    def deeper[_: P]: P[Int] = P(" ".repX(indent + 1).!.map(_.length))

    def block[_: P]: P[Seq[StatAST]] =
      P(
        "\n" ~~ deeper
          .flatMapX(i => new Parser(indent = i).statement.?.rep(1, sep = ("\n" + " " * i)./))
          .map(_.filter(_.isDefined).map(_.get)))

    def blockExpression[_: P]: P[ExprAST] = P(block map BlockExpr)

    def expressionOrBlock[_: P]: P[ExprAST] = P(expression | blockExpression)

    def optElse[_: P]: P[Option[ExprAST]] = P((nl.? ~ "else" ~ expressionOrBlock).?)

    def construct[_: P]: P[ExprAST] =
      P(
        "if" ~ (Index ~ condition ~ ("then" ~ expression | "then".? ~ blockExpression) ~ optElse)
          .map(ConditionalExpr.tupled) |
          ((ident ~ ":").? ~ "while" ~ Index ~ condition ~ ("do" ~ expression | "do".? ~ blockExpression) ~ optElse)
            .map(WhileExpr.tupled) |
          (Index ~ "break" ~ ident.? ~ ("(" ~ expression ~ ")").?).map(BreakExpr.tupled) |
          (Index ~ "continue" ~ ident.?).map(ContinueExpr.tupled) |
          condition
      )

    def condition[_: P]: P[ExprAST] = P(NoCut(disjunctive))

    def disjunctive[_: P]: P[ExprAST] =
      P(Index ~ NoCut(conjunctive) ~ (kw("or").! ~/ Index ~ conjunctive).rep).map(leftInfix)

    def conjunctive[_: P]: P[ExprAST] = P(Index ~ not ~ (kw("and").! ~/ Index ~ not).rep).map(leftInfix)

    def not[_: P]: P[ExprAST] =
      P(
        (kw("not").! ~ Index ~ not).map(PrefixExpr.tupled) |
          comparitive
      )

    def comparitive[_: P]: P[ExprAST] =
      P(
        (Index ~ NoCut(additive) ~ (StringIn("<=", ">=", "!=", "<", ">", "==", "div").! ~/ Index ~ additive)
          .map(Predicate.tupled)
          .rep(1))
          .map(CompareExpr.tupled) | additive
      )

    def additive[_: P]: P[ExprAST] =
      P(Index ~ multiplicative ~ (StringIn("+", "-").! ~/ Index ~ multiplicative).rep).map(leftInfix)

    def multiplicative[_: P]: P[ExprAST] =
      P(Index ~ negative ~ (StringIn("*", "/").! ~/ Index ~ negative).rep).map(leftInfix)

    def negative[_: P]: P[ExprAST] = P((sym("-").! ~ Index ~ negative).map(PrefixExpr.tupled) | power)

    def power[_: P]: P[ExprAST] = P((Index ~ incdec ~ sym("^").! ~ Index ~ power).map(InfixExpr.tupled) | incdec)

    def incdec[_: P]: P[ExprAST] =
      P(
        ((sym("++") | sym("--")).! ~ Index ~ applicative).map(PrefixExpr.tupled) |
          (Index ~ applicative ~ (sym("++") | sym("--")).!).map(PostfixExpr.tupled) |
          applicative
      )

    def applicative[_: P]: P[ExprAST] =
      P(
        NoCut(
          (Index ~ dot ~ (Index ~ "(" ~/ (Index ~ expression).map(Arg.tupled).rep(sep = ","./) ~ ")")
            .map(Args.tupled)
            .rep(1)).map(ApplyExpr.tupled)) | dot)

    def dot[_: P]: P[ExprAST] = P((Index ~ primary ~ "." ~ ident).map(DotExpr.tupled) | primary)

    def primary[_: P]: P[ExprAST] =
      P(
        (kw("true") | kw("false")).!.map(BooleanExpr) |
          ident.map(SymExpr) |
          ((digit.repX ~~ "." ~~ digits | digits ~~ ".") ~~ (CharIn("eE").? ~~ CharIn("+\\-").? ~~ digits)).!.map(
            DecimalExpr) |
          digits.map(IntegerExpr) |
          kw("null").map(_ => NullExpr) |
          kw("()").map(_ => NullExpr) |
          NoCut("`" ~~/ interpolator.repX ~~ "`").map(InterpolatedStringExpr) |
          ("'" ~~ ("\\'" | !CharIn("'\n") ~~ AnyChar).rep.! ~~ "'").map(StringExpr) |
          "(" ~/ expression ~ ")"
      )

    def digit[_: P]: P[Unit] = CharIn("0-9")

    def digits[_: P]: P[String] = P(digit.repX(1).!)

    def interpolator[_: P]: P[ExprAST] =
      P(
        P("$$").map(_ => StringExpr("$")) |
          P("${") ~/ expression ~ "}" |
          P("$") ~/ ident.map(SymExpr) |
          CharsWhile(c => c != '`' && c != '$').!.filter(_.nonEmpty).map(StringExpr)./
      )

    def keyword[_: P]: P[Unit] =
      StringIn(
        "and",
        "break",
        "class",
        "continue",
        "def",
        "div",
        "do",
        "else",
        "elsif",
        "extends",
        "false",
        "for",
        "if",
        "match",
        "mod",
        "not",
        "null",
        "or",
        "then",
        "true",
        "val",
        "var",
        "while",
        "with"
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
  }

}
