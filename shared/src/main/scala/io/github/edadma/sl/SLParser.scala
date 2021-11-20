package io.github.edadma.sl

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

  def nl: Rule0 = rule(zeroOrMore(anyOf("\r\n")) ~ sp)

  def kw(s: String): Rule1[String] =
    rule(quiet(capture(str(s) ~ !CharPredicate.AlphaNum ~ sp) ~> ((s: String) => s.trim)))

  def sym(s: String): Rule1[String] = rule(quiet(capture(s) ~> ((s: String) => s.trim)))

  def sources: Rule1[SourcesAST] = rule(nl ~ statements ~ EOI ~> SourcesAST)

  def statements: Rule1[Seq[StatAST]] = rule(zeroOrMore(statement ~ nl))

  def varStatement: Rule1[VarStat] = rule("var" ~ ident ~ optional("=" ~ expression) ~> VarStat)

  def defStatement: Rule1[DefStat] = rule("def" ~ ident ~ parameters ~ "=" ~ expression ~> DefStat)

  def parameters: Rule1[Seq[Ident]] = rule("(" ~ zeroOrMore(ident).separatedBy(",") ~ ")" | push(Nil))

  def block: Rule1[ExprAST] = rule("{" ~ nl ~ zeroOrMore(statement ~ nl) ~ "}" ~> BlockExpr)

  def statement: Rule1[StatAST] =
    rule {
      varStatement | defStatement | expression ~> ExpressionStat
    }

  def expression: Rule1[ExprAST] = conditional

  def conditional: Rule1[ExprAST] =
    rule {
      ("if" ~ condition ~ "then" ~ conditional ~ optional("else" ~ conditional) ~> ConditionalExpr) | condition
    }

  def condition: Rule1[ExprAST] = disjunctive

  def disjunctive: Rule1[ExprAST] = rule(conjunctive ~ zeroOrMore("or" ~ conjunctive ~> OrExpr))

  def conjunctive: Rule1[ExprAST] = rule(not ~ zeroOrMore("and" ~ not ~> AndExpr))

  def not: Rule1[ExprAST] =
    rule {
      kw("not") ~ pos ~ not ~> PrefixExpr |
        comparitive
    }

  def comparitive: Rule1[ExprAST] =
    rule {
      pos ~ assignment ~ oneOrMore(
        (sym("<=") | sym(">=") | sym("!=") | sym("<") | sym(">") | sym("=") | kw("div")) ~ pos ~ assignment ~> RightOper) ~> CompareExpr | assignment
    }

  def assignment: Rule1[ExprAST] = rule(pos ~ additive ~ "=" ~ pos ~ additive ~> AssignmentExpr | additive)

  def additive: Rule1[ExprAST] =
    rule {
      pos ~ multiplicative ~ oneOrMore((sym("++") | sym("+") | sym("-")) ~ pos ~ multiplicative ~> RightOper) ~> LeftInfixExpr | multiplicative
    }

  def multiplicative: Rule1[ExprAST] =
    rule {
      pos ~ negative ~ oneOrMore(
        (sym("*") | sym("/") | kw("mod") | sym("\\")) ~
          pos ~ negative ~> RightOper) ~> LeftInfixExpr | negative
    }

  def negative: Rule1[ExprAST] = rule(sym("-") ~ pos ~ negative ~> PrefixExpr | power)

  def power: Rule1[ExprAST] = rule(pos ~ applicative ~ sym("^") ~ pos ~ power ~> RightInfixExpr | applicative)

  def applicative: Rule1[ExprAST] =
    rule(
      pos ~ primary ~ oneOrMore("(" ~ zeroOrMore(pos ~ expression ~> Arg).separatedBy(",") ~ ")" ~> Args) ~> ApplyExpr | primary)

  def primary: Rule1[ExprAST] = rule {
    boolean |
      decimal |
      integer |
      nul |
      variable |
      string |
      map |
      seq |
      block |
      "(" ~ expression ~ ")"
  }

  def map: Rule1[MapExpr] =
    rule("{" ~ zeroOrMore(ident ~ ":" ~ pos ~ expression ~> MapEntry).separatedBy(",") ~ "}" ~> MapExpr)

  def seq: Rule1[SeqExpr] = rule("[" ~ zeroOrMore(expression).separatedBy(",") ~ "]" ~> SeqExpr)

  def nul: Rule1[NullExpr.type] = rule("null" ~ push(NullExpr))

  def boolean: Rule1[BooleanExpr] =
    rule((kw("true") | kw("false")) ~> BooleanExpr)

  def decimal: Rule1[DecimalExpr] =
    rule {
      capture(
        (zeroOrMore(CharPredicate.Digit) ~ '.' ~ digits | digits ~ '.') ~
          optional((ch('e') | 'E') ~ optional(ch('+') | '-') ~ digits)
      ) ~ sp ~> DecimalExpr
    }

  def integer: Rule1[IntegerExpr] = rule(capture(digits) ~ sp ~> IntegerExpr)

  def digits: Rule0 = rule(oneOrMore(CharPredicate.Digit))

  def variable: Rule1[VarExpr] = rule(ident ~> VarExpr)

  def string: Rule1[StringExpr] = rule((singleQuoteString | doubleQuoteString) ~> StringExpr)

  def singleQuoteString: Rule1[String] = rule('\'' ~ capture(zeroOrMore("\\'" | noneOf("'\n"))) ~ '\'' ~ sp)

  def doubleQuoteString: Rule1[String] = rule('"' ~ capture(zeroOrMore("\\\"" | noneOf("\"\n"))) ~ '"' ~ sp)

  def ident: Rule1[Ident] =
    rule {
      pos ~ !("var" | "def" | "mod" | "if" | "then" | "true" | "false" | "null" | "elsif" | "with" | "match" | "case" | "for") ~ capture(
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
