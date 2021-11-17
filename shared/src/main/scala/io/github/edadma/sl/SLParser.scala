package io.github.edadma.sl

import io.github.edadma.char_reader.CharReader

import scala.util.{Failure, Success}
import org.parboiled2._

import scala.language.implicitConversions

class SLParser(val input: ParserInput) extends Parser {

  class Position(val offset: Int) {
    override def toString: String = offset.toString
  }

  private val delimiters = "[](){}`'\","
  private val delimiter = CharPredicate(delimiters)

  implicit def wsStr(s: String): Rule0 =
    if (delimiters.exists(_.toString == s))
      rule(str(s) ~ sp)
    else if (s.forall(!_.isLetterOrDigit))
      rule(str(s) ~ !(CharPredicate.Visible -- CharPredicate.AlphaNum -- delimiter) ~ sp)
    else
      rule(str(s) ~ !CharPredicate.AlphaNum ~ sp)

  def pos: Rule1[Position] = rule(push(new Position(cursor)))

  def sp: Rule0 = rule(quiet(zeroOrMore(anyOf(" \t"))))

  def nl: Rule0 = rule(zeroOrMore(anyOf("\r\n")))

  def kw(s: String): Rule1[String] =
    rule(quiet(capture(str(s) ~ !CharPredicate.AlphaNum ~ sp) ~> ((s: String) => s.trim)))

  def sym(s: String): Rule1[String] = rule(quiet(capture(s) ~> ((s: String) => s.trim)))

  def sources: Rule1[SourcesAST] = rule(nl ~ statements ~ EOI ~> SourcesAST)

  def statements: Rule1[Seq[StatAST]] = rule(oneOrMore(statement ~ nl))

  def varStatement: Rule1[VarStatAST] = rule("var" ~ ident ~ optional("=" ~ expression) ~> VarStatAST)

  def defStatement: Rule1[DefStatAST] = rule("def" ~ ident ~ parameters ~ "=" ~ (block | expression) ~> DefStatAST)

  def parameters: Rule1[Seq[Ident]] = rule("(" ~ zeroOrMore(ident).separatedBy(",") ~ ")" | push(Nil))

  def block: Rule1[ExprAST] = rule("{" ~ statements ~ "}" ~> BlockExprAST)

  def statement: Rule1[StatAST] =
    rule {
      varStatement | defStatement | expression ~> ExpressionStatAST
    }

  def expression: Rule1[ExprAST] = conditional

  def conditional: Rule1[ExprAST] =
    rule {
      ("if" ~ condition ~ "then" ~ conditional ~ optional("else" ~ conditional) ~> ConditionalAST) | condition
    }

  def condition: Rule1[ExprAST] = disjunctive

  def disjunctive: Rule1[ExprAST] =
    rule {
      conjunctive ~ zeroOrMore("or" ~ conjunctive ~> OrExpr)
    }

  def conjunctive: Rule1[ExprAST] =
    rule {
      not ~ zeroOrMore("and" ~ not ~> AndExpr)
    }

  def not: Rule1[ExprAST] =
    rule {
      kw("not") ~ pos ~ not ~> PrefixExpr |
        comparitive
    }

  def comparitive: Rule1[ExprAST] =
    rule {
      pos ~ applicative ~ oneOrMore(
        (sym("<=") | sym(">=") | sym("!=") | sym("<") | sym(">") | sym("=") | kw("div")) ~
          pos ~ applicative ~> Tuple3[String, Position, ExprAST] _) ~> CompareExpr |
        applicative
    }

  def applicative: Rule1[ExprAST] = rule(apply | additive)

  def expressions: Rule1[Seq[ExprAST]] = rule("(" ~ zeroOrMore(expression).separatedBy(",") ~ ")")

  def apply: Rule1[ApplyExpr] = rule(ident ~ expressions ~> ApplyExpr)

  def additive: Rule1[ExprAST] =
    rule {
      pos ~ multiplicative ~ oneOrMore((sym("++") | sym("+") | sym("-")) ~ pos ~ multiplicative ~> Tuple3[
        String,
        Position,
        ExprAST] _) ~> LeftInfixExpr | multiplicative
    }

  def multiplicative: Rule1[ExprAST] =
    rule {
      pos ~ negative ~ oneOrMore(
        (sym("*") | sym("/") | kw("mod") | sym("\\")) ~
          pos ~ negative ~> Tuple3[String, Position, ExprAST] _) ~> LeftInfixExpr | negative
    }

  def negative: Rule1[ExprAST] =
    rule {
      sym("-") ~ pos ~ negative ~> PrefixExpr |
        power
    }

  def power: Rule1[ExprAST] =
    rule {
      pos ~ index ~ sym("^") ~ pos ~ power ~> RightInfixExpr |
        index
    }

  def index: Rule1[ExprAST] =
    rule {
      primary ~ zeroOrMore(
        "[" ~ pos ~ expression ~ "]" ~> IndexExpr | test(!lastChar.isWhitespace) ~ '.' ~ ident ~> MethodExpr)
    }

  def primary: Rule1[ExprAST] = rule {
    boolean |
      number |
      nul |
      variable |
      string |
      map |
      seq |
      "(" ~ expression ~ ")"
  }

  def map: Rule1[MapExpr] =
    rule(
      "{" ~ zeroOrMore(ident ~ ":" ~ pos ~ expression ~> Tuple3[Ident, Position, ExprAST] _)
        .separatedBy(",") ~ "}" ~> MapExpr)

  def seq: Rule1[SeqExpr] = rule("[" ~ zeroOrMore(expression).separatedBy(",") ~ "]" ~> SeqExpr)

  def number: Rule1[NumberExpr] = rule(pos ~ decimal ~> NumberExpr)

  def nul: Rule1[NullExpr] = rule(pos ~ "null" ~> NullExpr)

  def boolean: Rule1[BooleanExpr] =
    rule(pos ~ (kw("true") | kw("false")) ~> ((p: Position, b: String) => BooleanExpr(p, b == "true")))

  def decimal: Rule1[BigDecimal] =
    rule {
      capture(
        (zeroOrMore(CharPredicate.Digit) ~ '.' ~ digits | digits ~ '.') ~
          optional((ch('e') | 'E') ~ optional(ch('+') | '-') ~ digits) |
          digits
      ) ~ sp ~> ((s: String) => BigDecimal(s))
    }

  def integer: Rule1[Int] = rule(capture(digits) ~ sp ~> ((s: String) => s.toInt))

  def digits: Rule0 = rule(oneOrMore(CharPredicate.Digit))

  def variable: Rule1[VarExpr] = rule(ident ~> VarExpr)

//  def element: Rule1[ElementExpr] =
//    rule(pos ~ zeroOrMore(identnsp).separatedBy(".") ~ sp ~> ElementExpr)

  def string: Rule1[StringExpr] =
    rule(pos ~ (singleQuoteString | doubleQuoteString) ~> ((p: Position, s: String) => StringExpr(p, s)))

  def singleQuoteString: Rule1[String] = rule('\'' ~ capture(zeroOrMore("\\'" | noneOf("'\n"))) ~ '\'' ~ sp)

  def doubleQuoteString: Rule1[String] = rule('"' ~ capture(zeroOrMore("\\\"" | noneOf("\"\n"))) ~ '"' ~ sp)

  def ident: Rule1[Ident] =
    rule {
      push(cursor) ~ !("if" | "then" | "true" | "false" | "null" | "elsif" | "with" | "match" | "case" | "for") ~ capture(
        (CharPredicate.Alpha | '_') ~ zeroOrMore(CharPredicate.AlphaNum | '_')) ~ sp ~> Ident
    }

  def parseSources: SourcesAST =
    sources.run() match {
      case Success(ast)           => ast
      case Failure(e: ParseError) => sys.error("parse error: " + formatError(e))
      case Failure(e)             => sys.error("Unexpected error during parsing run: " + e)
    }

  def parseExpression: ExprAST =
    expression.run() match {
      case Success(ast)           => ast
      case Failure(e: ParseError) => sys.error("Expression is not valid: " + formatError(e))
      case Failure(e)             => sys.error("Unexpected error during parsing run: " + e)
    }

}
