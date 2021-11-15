package io.github.edadma.sl

import io.github.edadma.char_reader.CharReader

import scala.util.{Failure, Success}
import org.parboiled2._

import scala.language.implicitConversions

class SLParser(val input: ParserInput) extends Parser {

  implicit def wsStr(s: String): Rule0 = rule(str(s) ~ sp)

  def kwcapture(s: String): Rule1[String] =
    rule(quiet(capture(str(s) ~ !CharPredicate.AlphaNum ~ sp) ~> ((s: String) => s.trim)))

  def kw(s: String): Rule0 = rule(quiet(str(s) ~ !CharPredicate.AlphaNum ~ sp))

  def sym(s: String): Rule1[String] = rule(quiet(capture(s) ~> ((s: String) => s.trim)))

  def parseExpression: ExprAST =
    expression.run() match {
      case Success(ast)           => ast
      case Failure(e: ParseError) => sys.error("Expression is not valid: " + formatError(e))
      case Failure(e)             => sys.error("Unexpected error during parsing run: " + e)
    }

  def identnsp: Rule1[Ident] =
    rule {
      push(cursor) ~ !("if" | "true" | "false" | "null" | "elsif" | "with" | "match" | "case" | "no" ~ "output") ~ capture(
        (CharPredicate.Alpha | '_') ~ zeroOrMore(CharPredicate.AlphaNum | '_')) ~> Ident
    }

  def ident: Rule1[Ident] = rule(identnsp ~ sp)

  def sp: Rule0 = rule(quiet(zeroOrMore(anyOf(" \t\r\n"))))

}
