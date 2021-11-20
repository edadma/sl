package io.github.edadma.sl

trait SLAST

case class Ident(pos: SLParser#Position, name: String)

case class SourcesAST(stats: Seq[StatAST]) extends SLAST

trait StatAST

trait DeclarationAST

case class DefStat(ident: Ident, params: Seq[Ident], body: ExprAST) extends StatAST with DeclarationAST

case class VarStat(ident: Ident, init: Option[ExprAST]) extends StatAST with DeclarationAST

case class ExpressionStat(expr: ExprAST) extends StatAST

trait ExprAST extends SLAST

case class BlockExpr(stats: Seq[StatAST]) extends ExprAST

case class InfixExpr(left: ExprAST, op: String, right: ExprAST) extends ExprAST

case class StringExpr(s: String) extends ExprAST

case class IntegerExpr(n: String) extends ExprAST

case class DecimalExpr(n: String) extends ExprAST

case class BooleanExpr(b: String) extends ExprAST

case object NullExpr extends ExprAST

case class VarExpr(name: Ident) extends ExprAST

case class MapEntry(key: Ident, pos: SLParser#Position, value: ExprAST)

case class MapExpr(pairs: Seq[MapEntry]) extends ExprAST

case class SeqExpr(elems: Seq[ExprAST]) extends ExprAST

case class PrefixExpr(op: String, pos: SLParser#Position, expr: ExprAST) extends ExprAST

case class RightOper(op: String, pos: SLParser#Position, expr: ExprAST)

case class LeftInfixExpr(lpos: SLParser#Position, left: ExprAST, right: Seq[RightOper]) extends ExprAST

case class RightInfixExpr(lpos: SLParser#Position, left: ExprAST, op: String, rpos: SLParser#Position, right: ExprAST)
    extends ExprAST

case class Arg(pos: SLParser#Position, expr: ExprAST)

case class Args(pos: SLParser#Position, args: Seq[Arg])

case class ApplyExpr(pos: SLParser#Position, expr: ExprAST, calls: Seq[Args]) extends ExprAST

case class ConditionalExpr(cond: ExprAST, yes: ExprAST, no: Option[ExprAST]) extends ExprAST

case class OrExpr(left: ExprAST, right: ExprAST) extends ExprAST

case class AndExpr(left: ExprAST, right: ExprAST) extends ExprAST

case class CompareExpr(lpos: SLParser#Position, left: ExprAST, right: Seq[RightOper]) extends ExprAST

case class AssignmentExpr(lpos: SLParser#Position, lvalue: ExprAST, rpos: SLParser#Position, expr: ExprAST)
    extends ExprAST

case class ReturnStat(expr: Option[ExprAST]) extends StatAST
