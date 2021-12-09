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
              case '#'        => rec(current + 1, state = 1)
              case _          => ctx.freshSuccessUnit(current)
            }
          case 1 =>
            if (currentChar == '\n')
              ctx.freshSuccessUnit(current)
            else
              rec(current + 1, 1)
          case 2 =>
            (currentChar: @switch) match {
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

    def sym[_: P, T](p: => P[T]): P[T] =
      P(p ~~ !CharPred(c => !c.isLetterOrDigit && !delimiters.contains(c) && !c.isWhitespace))

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
          .flatMapX(i => new Parser(indent = i).statement.?.rep(1, sep = "\n" ~~ ((" " * i) | Pass ~ &("\n" | End))))
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
          ((ident ~ ":").? ~ "do" ~ expressionOrBlock ~ nl.? ~ "while" ~ Index ~ condition ~ optElse)
            .map(DoWhileExpr.tupled) |
          (Index ~ "break" ~ ident.? ~ ("(" ~ expression ~ ")").?).map(BreakExpr.tupled) |
          (Index ~ "continue" ~ ident.?).map(ContinueExpr.tupled) |
          condition
      )

    def condition[_: P]: P[ExprAST] = P(disjunctive)

    def disjunctive[_: P]: P[ExprAST] = P(Index ~ conjunctive ~ (kw("or").! ~ Index ~ conjunctive).rep).map(leftInfix)

    def conjunctive[_: P]: P[ExprAST] = P(Index ~ not ~ (kw("and").! ~ Index ~ not).rep).map(leftInfix)

    def not[_: P]: P[ExprAST] =
      P(
        (kw("not").! ~ Index ~ not).map(PrefixExpr.tupled) |
          comparitive
      )

    def comparitive[_: P]: P[ExprAST] =
      P(
        (Index ~ or ~ (sym(StringIn("<=", ">=", "!=", "==", "<", ">", "div")).! ~ Index ~ or)
          .map(Predicate.tupled)
          .rep)
          .map {
            case (pos, left, Nil)   => left
            case (pos, left, right) => CompareExpr(pos, left, right)
          }
      )

    def or[_: P]: P[ExprAST] = P(Index ~ and ~ (sym("|").! ~ Index ~ and).rep).map(leftInfix)

    def and[_: P]: P[ExprAST] = P(Index ~ invert ~ (sym("&").! ~ Index ~ invert).rep).map(leftInfix)

    def invert[_: P]: P[ExprAST] = P((sym("~").! ~ Index ~ invert).map(PrefixExpr.tupled) | shift)

    def shift[_: P]: P[ExprAST] =
      P((Index ~ additive ~ (sym(StringIn("<<", ">>", ">>>")).! ~ Index ~ additive).rep).map(leftInfix))

    def additive[_: P]: P[ExprAST] =
      P((Index ~ multiplicative ~ (sym(StringIn("+", "-")).! ~ Index ~ multiplicative).rep).map(leftInfix))

    def multiplicative[_: P]: P[ExprAST] =
      P((Index ~ negative ~ (sym(StringIn("*", "/", "//", "\\")).! ~ Index ~ negative).rep).map(leftInfix))

    def negative[_: P]: P[ExprAST] = P((sym("-").! ~ Index ~ negative).map(PrefixExpr.tupled) | power)

    def power[_: P]: P[ExprAST] =
      P((Index ~ incdec ~ (sym("^") ~ Index ~ power).?).map {
        case (_, left, None)                   => left
        case (lpos, left, Some((rpos, right))) => InfixExpr(lpos, left, "^", rpos, right)
      })

    def incdec[_: P]: P[ExprAST] =
      P(
        (sym(StringIn("++", "--")).! ~ Index ~ applicative).map(PrefixExpr.tupled) |
          (Index ~ applicative ~ sym(StringIn("++", "--")).!.?).map {
            case (_, expr, None)       => expr
            case (pos, expr, Some(op)) => PostfixExpr(pos, expr, op)
          }
      )

    def applicative[_: P]: P[ExprAST] =
      P(
        (Index ~ primary ~ (("(" ~ (Index ~ expression).map(Arg.tupled).rep(sep = ",").map(Args) ~ ")") |
          ("." ~ ident).map(Dot)).rep).map {
          case (_, expr, Nil)   => expr
          case (pos, expr, ops) => ApplyExpr(pos, expr, ops)
        })

    def primary[_: P]: P[ExprAST] =
      P(
        (kw("true") | kw("false")).!.map(BooleanExpr) |
          ident.map(SymExpr) |
          ((digit.repX ~~ "." ~~ digits | digits ~~ ".") ~~ (CharIn("eE").? ~~ CharIn("+\\-").? ~~ digits)).!.map(
            DecimalExpr) |
          digits.map(IntegerExpr) |
          kw("null").map(_ => NullExpr) |
          P("()").map(_ => NullExpr) |
          ("`" ~~ interpolator.repX ~~ "`").map(InterpolatedStringExpr) |
          ("'" ~~ ("\\'" | !CharIn("'\n") ~~ AnyChar).repX.! ~~ "'").map(StringExpr) |
          ("\"" ~~ ("\\\"" | !CharIn("\"\n") ~~ AnyChar).repX.! ~~ "\"").map(StringExpr) |
          "{" ~ (expression ~ ":" ~ Index ~ expression).map(MapEntry.tupled).rep(sep = ",").map(MapExpr) ~ "}" |
          "[" ~ expression.rep(sep = ",").map(SeqExpr) ~ "]" |
          "(" ~ expression ~ ")")

    def digit[_: P]: P[Unit] = P(CharIn("0-9"))

    def digits[_: P]: P[String] = P(digit.repX(1).!)

    def interpolator[_: P]: P[ExprAST] =
      P(
        P("$$").map(_ => StringExpr("$")) |
          P("${") ~ expression ~ "}" |
          P("$") ~ ident.map(SymExpr) |
          CharsWhile(c => c != '`' && c != '$').!.filter(_.nonEmpty).map(StringExpr)
      )

    val keywords = Set(
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
    )

    def ident[_: P]: P[Ident] =
      P(
        (Index ~ (CharIn("a-zA-Z_") ~~ CharIn("a-zA-Z0-9_").repX).!)
          .filter(s => !keywords.contains(s._2))
          .map(Ident.tupled))

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
