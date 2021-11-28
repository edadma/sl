package io.github.edadma.sl

import scala.util.{Failure, Success}
import org.parboiled2._

import scala.language.implicitConversions

class SLParser(val input: ParserInput) extends Parser {

  private val delimiters = "[](){}`'\","
  private val delimiter = CharPredicate(delimiters)

  implicit def wsStr(s: String): Rule0 =
    if (delimiters.exists(_.toString == s))
      rule(str(s) ~ sp)
    else if (s.forall(!_.isLetterOrDigit))
      rule(str(s) ~ !(CharPredicate.Visible -- CharPredicate.AlphaNum -- delimiter) ~ sp)
    else
      rule(str(s) ~ !CharPredicate.AlphaNum ~ sp)

  def pos: Rule1[Cursor] = rule(push(new Cursor(cursor)))

  def sp: Rule0 = rule(quiet(zeroOrMore(anyOf(" \t") | '/' ~ '/' ~ zeroOrMore(noneOf("\n\r")))))

  def nl: Rule0 = rule(oneOrMore(anyOf("\r\n")))

  def nls: Rule0 = rule(zeroOrMore(anyOf("\r\n") ~ sp) ~ sp)

  def kw(s: String): Rule1[String] =
    rule(capture(str(s) ~ !CharPredicate.AlphaNum ~ sp) ~> ((s: String) => s.trim))

  def sym(s: String): Rule1[String] = rule(capture(s) ~> ((s: String) => s.trim))

  def sources: Rule1[SourcesAST] = rule(nls ~ statements ~ EOI ~> SourcesAST)

  def statements: Rule1[Seq[StatAST]] = rule(zeroOrMore(statement ~ nls))

  def classStatement: Rule1[ClassStat] = rule("class" ~ ident ~ parameters ~ block ~> ClassStat)

  def varStatement: Rule1[VarStat] = rule("var" ~ ident ~ optional("=" ~ expressionOrBlock) ~> VarStat)

  def defStatement: Rule1[DefStat] =
    rule("def" ~ ident ~ parameters ~ ("=" ~ expression | optional("=") ~ blockExpression) ~> DefStat)

  def parameters: Rule1[Seq[Ident]] = rule("(" ~ zeroOrMore(ident).separatedBy(",") ~ ")" | push(Nil))

  def block: Rule1[Seq[StatAST]] = rule(ch('\n') ~ '\ue000' ~ oneOrMore(statement ~ nls) ~ '\ue001')

  def blockExpression: Rule1[ExprAST] = rule(block ~> BlockExpr)

  def statement: Rule1[StatAST] =
    rule {
      classStatement | varStatement | defStatement | expression ~> ExpressionStat
    }

  def expression: Rule1[ExprAST] = rule(function)

  def functionParameters: Rule1[Seq[Ident]] =
    rule(ident ~> (id => Seq(id)) | "(" ~ zeroOrMore(ident).separatedBy(",") ~ ")")

  def function: Rule1[ExprAST] = rule(functionParameters ~ "->" ~ pos ~ expressionOrBlock ~> FunctionExpr | assignment)

  def assignment: Rule1[ExprAST] = rule(pos ~ applicative ~ "=" ~ pos ~ expressionOrBlock ~> AssignExpr | construct)

  def expressionOrBlock: Rule1[ExprAST] = rule(expression | blockExpression)

  def optElse: Rule1[Option[ExprAST]] = rule(optional(nls ~ "else" ~ expressionOrBlock))

  def construct: Rule1[ExprAST] =
    rule {
      "if" ~ pos ~ condition ~ ("then" ~ expression | optional("then") ~ blockExpression) ~ optElse ~> ConditionalExpr |
        "while" ~ pos ~ condition ~ ("do" ~ expression | optional("do") ~ blockExpression) ~ optElse ~> WhileExpr |
        "break" ~ optional(ident) ~ optional(condition) ~> BreakExpr |
        "continue" ~ optional(ident) ~> ContinueExpr |
        condition
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
      pos ~ additive ~ oneOrMore(
        (sym("<=") | sym(">=") | sym("!=") | sym("<") | sym(">") | sym("==") | kw("div")) ~ pos ~ additive ~> RightOper) ~> CompareExpr | additive
    }

  def additive: Rule1[ExprAST] =
    rule {
      pos ~ multiplicative ~ oneOrMore((sym("+") | sym("-")) ~ pos ~ multiplicative ~> RightOper) ~> LeftInfixExpr | multiplicative
    }

  def multiplicative: Rule1[ExprAST] =
    rule {
      pos ~ negative ~ oneOrMore(
        (sym("*") | sym("/") | kw("mod") | sym("\\")) ~
          pos ~ negative ~> RightOper) ~> LeftInfixExpr | negative
    }

  def negative: Rule1[ExprAST] = rule(sym("-") ~ pos ~ negative ~> PrefixExpr | power)

  def power: Rule1[ExprAST] = rule(pos ~ incdec ~ sym("^") ~ pos ~ power ~> RightInfixExpr | incdec)

  def incdec: Rule1[ExprAST] =
    rule {
      (sym("++") | sym("--")) ~ pos ~ applicative ~> PrefixExpr |
        pos ~ applicative ~ (sym("++") | sym("--")) ~> PostfixExpr |
        applicative
    }

  def applicative: Rule1[ExprAST] =
    rule(
      pos ~ dot ~ oneOrMore(pos ~ "(" ~ zeroOrMore(pos ~ expression ~> Arg).separatedBy(",") ~ ")" ~> Args) ~> ApplyExpr | dot)

  def dot: Rule1[ExprAST] = rule(pos ~ primary ~ "." ~ ident ~> DotExpr | primary)

  def primary: Rule1[ExprAST] = rule {
    (kw("true") | kw("false")) ~> BooleanExpr |
      capture(
        (zeroOrMore(CharPredicate.Digit) ~ '.' ~ digits | digits ~ '.') ~
          optional((ch('e') | 'E') ~ optional(ch('+') | '-') ~ digits)
      ) ~ sp ~> DecimalExpr |
      capture(digits) ~ sp ~> IntegerExpr |
      "null" ~ push(NullExpr) |
      "()" ~ push(VoidExpr) |
      ident ~> SymExpr |
      '\'' ~ capture(zeroOrMore("\\'" | noneOf("'\n"))) ~ '\'' ~ sp ~> StringExpr |
      '"' ~ capture(zeroOrMore("\\\"" | noneOf("\"\n"))) ~ '"' ~ sp ~> StringExpr |
      "{" ~ zeroOrMore(ident ~ ":" ~ pos ~ expression ~> MapEntry).separatedBy(",") ~ "}" ~> MapExpr |
      "[" ~ zeroOrMore(expression).separatedBy(",") ~ "]" ~> SeqExpr |
      "(" ~ expression ~ ")"
  }

  def digits: Rule0 = rule(oneOrMore(CharPredicate.Digit))

  def keyword: Rule0 =
    rule(
      "div" | "and" | "or" | "not" | "break" | "continue" | "var" | "val" | "def" | "mod" | "if" | "then" | "true" | "false" | "null" | "else" | "elsif" | "with" | "extends" | "class" | "module" | "match" | "case" | "for" | "do" | "while")

  // todo: code more efficient way of checking if an ident is a keyword

  def ident: Rule1[Ident] =
    rule {
      pos ~ /*!keyword ~*/ capture((CharPredicate.Alpha | '_') ~ zeroOrMore(CharPredicate.AlphaNum | '_')) ~ sp ~> Ident
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
