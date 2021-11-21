package io.github.edadma.sl

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Compiler {

  def declarations(stats: Seq[StatAST]): Seq[DeclarationAST] = {
    val declrs = stats filter (_.isInstanceOf[DeclarationAST])

    declrs.asInstanceOf[Seq[DeclarationAST]]
  }

  def compileBlock(stats: Seq[StatAST],
                   buf: ArrayBuffer[Inst] = new ArrayBuffer,
                   fixups: mutable.Stack[Int] = new mutable.Stack): ArrayBuffer[Inst] = {
    def compileExpr(pos: SLParser#Position, expr: ExprAST): Unit = {
      def forward: Int = {
        val len = buf.length

        buf += null
        len
      }

      def patch(fixup: Int): Unit = buf(fixup) = SLInteger(buf.length - fixup)

      def loop(start: Int): Unit = {
        buf += SLInteger(-(buf.length - start) - 2)
        buf += BranchInst
      }

      if (pos ne null)
        buf += PosInst(pos)

      expr match {
        case CompareExpr(lpos, left, right) =>
          compileExpr(lpos, left)
          right foreach {
            case RightOper(op, pos, expr) =>
              compileExpr(pos, expr)
              buf += (op match {
                case "<=" => LteInst
                case "<"  => LtInst
              })
          }
        case BlockExpr(stats) => compileBlock(stats, buf, fixups)
        case SymExpr(ident)   => buf ++= Seq(PosInst(ident.pos), SLString(ident.name), SymInst)
        case IntegerExpr(n)   => buf += SLNumber(n.toDouble)
        case DecimalExpr(n)   => buf += SLNumber(n.toDouble)
        case StringExpr(s)    => buf += SLString(s)
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
        case WhileExpr(pos, cond, body, no) =>
          val start = buf.length

          compileExpr(pos, cond)

          val exit = forward

          buf += BranchIfFalseInst
          compileExpr(null, body)
          loop(start)
          patch(exit)
        case ConditionalExpr(pos, cond, yes, no) =>
          compileExpr(pos, cond)

          val jumpno = forward

          buf += BranchIfFalseInst
          compileExpr(null, yes)

          val jumpyes = forward

          patch(jumpno)

          no foreach { e =>
            compileExpr(null, e)
            patch(jumpyes)
          }
      }
    }

    stats foreach {
      case DefStat(ident, params, body) =>
      case VarStat(ident, init)         =>
      case ExpressionStat(expr)         => compileExpr(null, expr)
    }

    buf
  }

  def apply(ast: SLAST): CodeBlock =
    ast match {
      case SourcesAST(stats) => new CodeBlock(compileBlock(stats))
//      case BlockExpr(stats)  => new CodeBlock(compileBlock(stats))
    }

}
