package io.github.edadma.sl

import scala.collection.mutable.ArrayBuffer

object Compiler {

  def declarations(stats: Seq[StatAST]): Seq[DeclarationAST] = {
    val declrs = stats filter (_.isInstanceOf[DeclarationAST])

    declrs.asInstanceOf[Seq[DeclarationAST]]
  }

  def compileBlock(stats: Seq[StatAST]): CodeBlock = {
    val buf = new ArrayBuffer[Inst]

    def compileExpr(pos: SLParser#Position, expr: ExprAST): Unit = {
      if (pos ne null)
        buf += PosInst(pos)

      expr match {
        case IntegerExpr(n) => buf += SLNumber(n.toDouble)
        case DecimalExpr(n) => buf += SLNumber(n.toDouble)
        case LeftInfixExpr(lpos, left, right) =>
          compileExpr(lpos, left)
          right foreach {
            case RightOper(op, pos, expr) =>
              compileExpr(pos, expr)
              buf +=
                (op match {
                  case "+" => AddInst
                  case "-" => SubInst
                })
          }
        case AssignmentExpr(lpos, lvalue, rpos, expr) =>
          compileExpr(lpos, lvalue)
          buf += MutableInst
          compileExpr(rpos, expr)
          buf += AssignInst
        case ApplyExpr(fpos, expr, calls) =>
          def generateCall(as: Args): Unit = {
            buf += CallableInst
            as.args foreach {
              case Arg(pos, expr) => compileExpr(pos, expr)
            }
            buf += SLInteger(as.args.length)
            // todo: as.pos wasn't pushed
            buf += CallInst
          }

          compileExpr(fpos, expr)
          generateCall(calls.head)

          for (a <- calls.tail) {
            buf += CallableInst
            generateCall(a)
          }
      }
    }

    stats foreach {
      case DefStat(ident, params, body) =>
      case VarStat(ident, init)         =>
      case ExpressionStat(expr)         => compileExpr(null, expr)
    }

    new CodeBlock(buf)
  }

  def apply(ast: SLAST): CodeBlock =
    ast match {
      case SourcesAST(stats) => compileBlock(stats)
      case BlockExpr(stats)  => compileBlock(stats)
    }

}
