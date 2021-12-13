package io.github.edadma.sl

trait SLAST

case class Ident(pos: Int, name: String)

case class ModuleAST(stats: Seq[StatAST]) extends SLAST

trait StatAST

trait DeclarationAST

case class ClassStat(ident: Ident, params: Seq[Ident], body: Seq[StatAST]) extends StatAST with DeclarationAST

case class DefStat(ident: Ident, params: Seq[Ident], body: ExprAST) extends StatAST with DeclarationAST

case class VarStat(ident: Ident, init: Option[ExprAST]) extends StatAST with DeclarationAST {
  var initialized: Boolean = false
}

case class ValStat(ident: Ident, init: ExprAST, var initialized: Boolean = false) extends StatAST with DeclarationAST

case class ExpressionStat(expr: ExprAST) extends StatAST

case class BreakExpr(pos: Int, label: Option[Ident], expr: Option[ExprAST]) extends ExprAST

case class ContinueExpr(pos: Int, label: Option[Ident]) extends ExprAST

trait ExprAST extends SLAST

case class FunctionExpr(params: Seq[Ident], pos: Int, body: ExprAST) extends StatAST with ExprAST

case class BlockExpr(stats: Seq[StatAST]) extends ExprAST

case class InterpolatedStringExpr(exprs: Seq[ExprAST]) extends ExprAST

case class StringExpr(s: String) extends ExprAST

case class NumberExpr(n: String) extends ExprAST

case class BooleanExpr(b: String) extends ExprAST

case object NullExpr extends ExprAST

case object VoidExpr extends ExprAST

case class SymExpr(ident: Ident) extends ExprAST

case class MapEntry(key: ExprAST, pos: Int, value: ExprAST)

case class MapExpr(entries: Seq[MapEntry]) extends ExprAST

case class SeqExpr(elems: Seq[ExprAST]) extends ExprAST

case class PrefixExpr(op: String, pos: Int, expr: ExprAST) extends ExprAST

case class PostfixExpr(pos: Int, expr: ExprAST, op: String) extends ExprAST

case class Predicate(op: String, pos: Int, expr: ExprAST)

case class InfixExpr(lpos: Int, left: ExprAST, op: String, rpos: Int, right: ExprAST) extends ExprAST

case class ApplyExpr(pos: Int, expr: ExprAST, ops: Seq[Applicative]) extends ExprAST

trait Applicative

case class Arg(pos: Int, expr: ExprAST)

case class Args(args: Seq[Arg]) extends Applicative

case class Dot(elem: Ident) extends Applicative

case class ConditionalExpr(pos: Int, cond: ExprAST, yes: ExprAST, no: Option[ExprAST]) extends ExprAST

case class WhileExpr(label: Option[Ident], pos: Int, cond: ExprAST, body: ExprAST, els: Option[ExprAST]) extends ExprAST

case class ForExpr(label: Option[Ident], index: Ident, pos: Int, iterable: ExprAST, body: ExprAST, els: Option[ExprAST])
    extends ExprAST { var it: String = _ }

case class DoWhileExpr(label: Option[Ident], body: ExprAST, pos: Int, cond: ExprAST, els: Option[ExprAST])
    extends ExprAST

case class OrExpr(left: ExprAST, right: ExprAST) extends ExprAST

case class AndExpr(left: ExprAST, right: ExprAST) extends ExprAST

case class CompareExpr(lpos: Int, left: ExprAST, right: Seq[Predicate]) extends ExprAST

case class AssignExpr(lpos: Int, lvalue: ExprAST, rpos: Int, expr: ExprAST) extends ExprAST

case class ReturnStat(expr: Option[ExprAST]) extends StatAST
