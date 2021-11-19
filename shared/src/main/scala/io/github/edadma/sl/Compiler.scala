package io.github.edadma.sl

import scala.collection.mutable.ArrayBuffer

object Compiler {

  def declarations(stats: Seq[StatAST]): Seq[DeclarationAST] = {
    val declrs = stats filter (_.isInstanceOf[DeclarationAST])

    declrs.asInstanceOf[Seq[DeclarationAST]]
  }

  def compileBlock(stats: Seq[StatAST]): CodeBlock = {
    val buf = new ArrayBuffer[Inst]

    def compileExpr(expr: ExprAST): Unit =
      expr match {
        case IntegerExpr(n) => buf += Push(SLNumber(n.toDouble))
        case DecimalExpr(n) => buf += Push(SLNumber(n.toDouble))
        case LeftInfixExpr(lpos, left, right) =>
          compileExpr(left)
          right foreach {
            case RightOper(op, pos, expr) =>
              compileExpr(expr)
              buf +=
                (op match {
                  case "+" => Add
                  case "-" => Add
                })
          }
        case AssignmentExpr(name, expr) =>
        case ApplyExpr(expr, args)      =>
      }

    stats foreach {
      case DefStat(ident, params, body) =>
      case VarStat(ident, init)         =>
      case ExpressionStat(expr)         => compileExpr(expr)
    }

    new CodeBlock(buf)
  }

  def apply(ast: SLAST): CodeBlock =
    ast match {
      case SourcesAST(stats) => compileBlock(stats)
      case BlockExpr(stats)  => compileBlock(stats)
    }

}
