package io.github.edadma.sl

trait SLAST

case class Ident(pos: Int, name: String)

trait StatAST

case class SourcesAST(stats: Seq[StatAST]) extends SLAST

case class VarStatAST(ident: Ident, init: Option[ExprAST]) extends StatAST

case class ExpressionStatAST(expr: ExprAST) extends StatAST

case class DefStatAST(ident: Ident, params: Seq[Ident], body: ExprAST) extends StatAST

trait ExprAST extends SLAST

case class BlockExprAST(stats: Seq[StatAST]) extends ExprAST

case class InfixExprAST(left: ExprAST, op: String, right: ExprAST) extends ExprAST

case class StringExpr(pos: SLParser#Position, s: String) extends ExprAST

case class NumberExpr(pos: SLParser#Position, n: BigDecimal) extends ExprAST

case class BooleanExpr(pos: SLParser#Position, b: Boolean) extends ExprAST

case class NullExpr(pos: SLParser#Position) extends ExprAST

case class VarExpr(name: Ident) extends ExprAST

case class ElementExpr(pos: SLParser#Position, ids: Seq[Ident]) extends ExprAST

case class MapExpr(pairs: Seq[(Ident, SLParser#Position, ExprAST)]) extends ExprAST

case class SeqExpr(elems: Seq[ExprAST]) extends ExprAST

case class PrefixExpr(op: String, pos: SLParser#Position, expr: ExprAST) extends ExprAST

case class LeftInfixExpr(lpos: SLParser#Position, left: ExprAST, right: Seq[(String, SLParser#Position, ExprAST)])
    extends ExprAST

case class RightInfixExpr(lpos: SLParser#Position, left: ExprAST, op: String, rpos: SLParser#Position, right: ExprAST)
    extends ExprAST

case class ApplyExpr(name: Ident, args: Seq[ExprAST]) extends ExprAST

case class ConditionalAST(cond: ExprAST, yes: ExprAST, no: Option[ExprAST]) extends ExprAST

case class OrExpr(left: ExprAST, right: ExprAST) extends ExprAST

case class AndExpr(left: ExprAST, right: ExprAST) extends ExprAST

case class CompareExpr(lpos: SLParser#Position, left: ExprAST, right: Seq[(String, SLParser#Position, ExprAST)])
    extends ExprAST

case class MethodExpr(expr: ExprAST, method: Ident) extends ExprAST

case class IndexExpr(expr: ExprAST, pos: SLParser#Position, index: ExprAST) extends ExprAST

case class PipeExpr(left: ExprAST, right: ApplyExpr) extends ExprAST

case class NonStrictExpr(expr: ExprAST) extends ExprAST

case class AssignmentAST(name: Ident, expr: ExprAST) extends SLAST

case class ReturnAST(expr: Option[ExprAST]) extends SLAST
