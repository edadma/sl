package io.github.edadma.sl

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class Compilation {

  val decls = new mutable.HashMap[String, DeclarationAST]

  def declarations(stats: Seq[StatAST]): Seq[DeclarationAST] =
    stats foreach {
      case DefStat(ident, params, body)   =>
      case VarStat(ident, init)           =>
      case ClassStat(ident, params, body) =>
    }

  trait BooleanCompilation
  case object BranchFalse extends BooleanCompilation
  case object BranchTrue extends BooleanCompilation

  private var buf: ArrayBuffer[Inst] = new ArrayBuffer

  def code: Code = new Code(buf)

  def compileExpr(pos: Cursor,
                  expr: ExprAST,
                  bool: BooleanCompilation = null,
                  lvalue: Boolean = false): ArrayBuffer[Inst] = {
    def forward(br: Inst): Int = {
      val len = buf.length

      buf += null
      buf += br
      len
    }

    def patch(fixup: Int): Unit = buf(fixup) = SLInteger(buf.length - fixup - 2)

    def loop(start: Int): Unit = {
      buf += SLInteger(-(buf.length - start) - 2)
      buf += BranchInst
    }

    if (pos ne null)
      buf += PosInst(pos)

    expr match {
      case BooleanExpr(b) => buf += SLBoolean(b == "true")
      case PrefixExpr("-", pos, expr) =>
        compileExpr(pos, expr)
        buf += NegInst
      case PrefixExpr(op @ ("++" | "--"), pos, expr) =>
        compileExpr(pos, expr)
        buf += MutableInst
        buf += DupInst
        buf += DupInst
        buf += SLValue.ONE
        buf += (if (op == "++") AddInst else SubInst)
        buf += AssignInst
      case PostfixExpr(pos, expr, op @ ("++" | "--")) =>
        compileExpr(pos, expr)
        buf += MutableInst
        buf += DupInst
        buf += DerefInst
        buf += SwapInst
        buf += DupInst
        buf += SLValue.ONE
        buf += (if (op == "++") AddInst else SubInst)
        buf += AssignInst
      case FunctionExpr(params, pos, body) =>
        buf += SLDefinedFunction("*anonymous*", new Compilation {
          compileExpr(pos, body)
          buf += RetInst
        }.code, params map (_.name))
      case VoidExpr => buf += SLVoid
      case CompareExpr(lpos, left, right) =>
        val fixups = new ListBuffer[Int]

        compileExpr(lpos, left)

        right.zipWithIndex foreach {
          case (RightOper(op, pos, expr), idx) =>
            val last = idx == right.length - 1

            compileExpr(pos, expr)

            if (!last) {
              buf += SwapInst
              buf += OverInst
            }

            op match {
              case "<="  => buf += LteInst
              case "<"   => buf += LtInst
              case "=="  => buf += EqInst
              case "div" => buf ++= Seq(SwapInst, ModInst, SLValue.ZERO, EqInst)
            }

            if (!last) {
              fixups += forward(BranchIfFalseCompareInst)
            }
        }

        fixups foreach patch
      case BlockExpr(stats) => compileBlock(stats)
      case SymExpr(ident)   => buf ++= Seq(PosInst(ident.pos), SLString(ident.name), if (lvalue) LvalueInst else SymInst)
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
                case "+"   => AddInst
                case "-"   => SubInst
                case "*"   => MulInst
                case "/"   => DivInst
                case "mod" => ModInst
              })
        }
      case AssignmentExpr(lpos, lvalue, rpos, expr) =>
        compileExpr(rpos, expr)
        compileExpr(lpos, lvalue, lvalue = true)
        buf += MutableInst
        buf += OverInst
        buf += AssignInst
      case ApplyExpr(fpos, expr, calls) =>
        def generateCall(as: Args): Unit = {
          buf += CallableInst
          as.args foreach {
            case Arg(pos, expr) =>
              compileExpr(pos, expr)
              buf += DerefInst
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

        val exit = forward(BranchIfFalseInst)

        compileExpr(null, body)
        buf += DropInst
        loop(start)
        patch(exit)

        if (no.isDefined)
          compileExpr(null, no.get)
        else
          buf += SLVoid
      case ConditionalExpr(pos, cond, yes, no) =>
        compileExpr(pos, cond)

        val jumpno = forward(BranchIfFalseInst)

        compileExpr(null, yes)

        val jumpyes = forward(BranchInst) // if (no.isDefined) forward(BranchInst) else 0

        patch(jumpno)

        no foreach { e =>
          compileExpr(null, e)
          patch(jumpyes)
        }

        if (no.isEmpty) {
          buf += SLVoid
          patch(jumpyes)
        }
    }

    buf
  }

  def compileStats(stats: Seq[StatAST]): Unit =
    stats foreach { s =>
      compileStat(s)

      if (s.isInstanceOf[ExpressionStat])
        buf += DropInst
    }

  def compileStat(stat: StatAST): Unit =
    stat match {
      case ClassStat(ident, params, stats) =>
        buf += SLString(ident.name)
        buf += DefinedClass(ident.name, Nil, new Compilation {
          compileStats(stats)
        }.code, params map (_.name))
        buf += ConstInst
      case DefStat(ident, params, body) =>
        buf += SLString(ident.name)
        buf += SLDefinedFunction(ident.name, new Compilation {
          compileExpr(null, body)
          buf += RetInst
        }.code, params map (_.name))
        buf += ConstInst
      case VarStat(ident, init) =>
      case ExpressionStat(expr) => compileExpr(null, expr)
    }

  def compileBlock(stats: Seq[StatAST]): ArrayBuffer[Inst] = {
    if (stats.nonEmpty) {
      compileStats(stats.init)
      compileStat(stats.last)
    }

    buf
  }

}

object Compilation {

  def apply(sources: SourcesAST): Code = new Compilation { compileStats(sources.stats) }.code

}
