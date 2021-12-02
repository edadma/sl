package io.github.edadma.sl

trait SLAST

case class Ident(pos: Cursor, name: String)

case class SourcesAST(stats: Seq[StatAST]) extends SLAST

trait StatAST

trait DeclarationAST

case class ClassStat(ident: Ident, params: Seq[Ident], body: Seq[StatAST]) extends StatAST with DeclarationAST

case class DefStat(ident: Ident, params: Seq[Ident], body: ExprAST) extends StatAST with DeclarationAST

case class VarStat(ident: Ident, init: Option[ExprAST]) extends StatAST with DeclarationAST {
  var initialized: Boolean = false
}

case class ValStat(ident: Ident, init: ExprAST, var initialized: Boolean = false) extends StatAST with DeclarationAST

case class ExpressionStat(expr: ExprAST) extends StatAST

case class BreakStat(label: Option[Ident], expr: Option[ExprAST]) extends StatAST

case class ContinueStat(label: Option[Ident]) extends StatAST

case class LabelStat(label: Ident) extends StatAST

trait ExprAST extends SLAST

case class FunctionExpr(params: Seq[Ident], pos: Cursor, body: ExprAST) extends StatAST with ExprAST

case class BlockExpr(stats: Seq[StatAST]) extends ExprAST

case class InfixExpr(left: ExprAST, op: String, right: ExprAST) extends ExprAST

case class InterpolatedStringExpr(exprs: Seq[ExprAST]) extends ExprAST

case class StringExpr(s: String) extends ExprAST

case class IntegerExpr(n: String) extends ExprAST

case class DecimalExpr(n: String) extends ExprAST

case class BooleanExpr(b: String) extends ExprAST

case object NullExpr extends ExprAST

case object VoidExpr extends ExprAST

case class SymExpr(ident: Ident) extends ExprAST

case class MapEntry(key: ExprAST, pos: Cursor, value: ExprAST)

case class MapExpr(entries: Seq[MapEntry]) extends ExprAST

case class SeqExpr(elems: Seq[ExprAST]) extends ExprAST

case class PrefixExpr(op: String, pos: Cursor, expr: ExprAST) extends ExprAST

case class PostfixExpr(pos: Cursor, expr: ExprAST, op: String) extends ExprAST

case class RightOper(op: String, pos: Cursor, expr: ExprAST)

case class LeftInfixExpr(lpos: Cursor, left: ExprAST, right: Seq[RightOper]) extends ExprAST

case class RightInfixExpr(lpos: Cursor, left: ExprAST, op: String, rpos: Cursor, right: ExprAST) extends ExprAST

case class Arg(pos: Cursor, expr: ExprAST)

case class Args(pos: Cursor, args: Seq[Arg])

case class ApplyExpr(pos: Cursor, expr: ExprAST, calls: Seq[Args]) extends ExprAST

case class DotExpr(pos: Cursor, expr: ExprAST, elem: Ident) extends ExprAST

case class ConditionalExpr(pos: Cursor, cond: ExprAST, yes: ExprAST, no: Option[ExprAST]) extends ExprAST

case class WhileExpr(label: Option[Ident], pos: Cursor, cond: ExprAST, body: ExprAST, no: Option[ExprAST])
    extends ExprAST

case class OrExpr(left: ExprAST, right: ExprAST) extends ExprAST

case class AndExpr(left: ExprAST, right: ExprAST) extends ExprAST

case class CompareExpr(lpos: Cursor, left: ExprAST, right: Seq[RightOper]) extends ExprAST

case class AssignExpr(lpos: Cursor, lvalue: ExprAST, rpos: Cursor, expr: ExprAST) extends ExprAST

case class ReturnStat(expr: Option[ExprAST]) extends StatAST
