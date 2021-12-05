package io.github.edadma.sl

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class Compilation {
  val NOPOS = -1
  val decls = new mutable.HashMap[String, DeclarationAST]

  trait BooleanCompilation
  case object BranchFalse extends BooleanCompilation
  case object BranchTrue extends BooleanCompilation

  protected val buf: ArrayBuffer[Inst] = new ArrayBuffer
  protected val loops = new mutable.Stack[Loop]

  protected class Loop {
    val start: Int = buf.length
    val breaks = new ListBuffer[Int]

    def addBreak(): Unit = {
      breaks += buf.length
      buf += null
      buf += BranchInst
    }

    def patchBreaks(): Unit = breaks foreach patch
  }

  def patch(fixup: Int): Unit = buf(fixup) = SLInteger(buf.length - fixup - 2)

  def loop(start: Int): Unit = {
    buf += SLInteger(-(buf.length - start) - 2)
    buf += BranchInst
  }

  def code: Code = new Code(buf)

  def compileDecls(stats: Seq[StatAST]): Unit = {
    def duplicate(pos: Int, name: String): Unit =
      if (decls contains name)
        problem(pos, s"duplicate declaration: $name")

    stats foreach {
      case d @ DefStat(Ident(pos, name), params, body) =>
        duplicate(pos, name)
        buf += SLString(name)
        buf += FunctionInst(name, new Compilation {
          compileExpr(NOPOS, body)
          buf += RetInst
        }.code, params map (_.name))
        buf += ConstInst
        decls(name) = d
      case ClassStat(Ident(pos, name), params, body) =>
        duplicate(pos, name)
        buf += SLString(name)
        buf += ClassInst(name, new Compilation {
          compileDecls(body)
          compileStats(body)
          buf += InstanceInst
          buf += RetInst
        }.code, params map (_.name))
        buf += ConstInst
      case d @ VarStat(Ident(pos, name), _) =>
        duplicate(pos, name)
        decls(name) = d
      case d @ ValStat(Ident(pos, name), _, _) =>
        duplicate(pos, name)
        decls(name) = d
      case ExpressionStat(AssignExpr(lpos, SymExpr(Ident(pos, name)), rpos, expr)) =>
        decls get name match {
          case None =>
            decls(name) = ValStat(Ident(pos, name), null)
          case Some(_: VarStat) =>
          case _                => problem(pos, s"symbol not variable: $name")
        }
      case _ =>
    }
  }

  def compileExpr(pos: Int,
                  expr: ExprAST,
                  bool: BooleanCompilation = null,
                  lvalue: Boolean = false): ArrayBuffer[Inst] = {
    def forward(br: Inst): Int = {
      val len = buf.length

      buf += null
      buf += br
      len
    }

    if (pos == NOPOS)
      buf += PosInst(pos)

    expr match {
      case NullExpr => buf += SLNull
      case BreakExpr(pos, label, expr) =>
        if (loops.isEmpty)
          problem(pos, "break not inside a loop construct")

        loops.top.addBreak()
      case InterpolatedStringExpr(exprs) =>
        buf += StringBuilderInst

        for (int <- exprs) {
          compileExpr(NOPOS, int)
          buf += AppendInst
        }

        buf += StringFromBuilderInst
      case MapExpr(entries) =>
        buf += SLValue.EMPTY

        val keys = new mutable.HashSet[String]

        entries foreach {
          case MapEntry(SymExpr(Ident(kpos, name)), pos, value) =>
            if (keys contains name)
              problem(kpos, s"duplicate map key: $name")

            keys += name
            buf += SLString(name)
            compileExpr(pos, value)
            buf += MapInsertInst
          case MapEntry(key, pos, value) =>
            compileExpr(NOPOS, key)
            compileExpr(pos, value)
            buf += MapInsertInst
        }
      case DotExpr(pos, expr, Ident(epos, elem)) =>
        compileExpr(pos, expr)
        buf += PosInst(epos)
        buf += SLString(elem)
        buf += DotInst
      case SeqExpr(elems) =>
        buf += SLValue.NIL

        for (e <- elems.reverse) {
          compileExpr(NOPOS, e)
          buf += ListPrependInst
        }
      case BooleanExpr(b) => buf += SLBoolean(b == "true")
      case PrefixExpr("-", pos, expr) =>
        compileExpr(pos, expr)
        buf += NegInst
      case PrefixExpr(op @ ("++" | "--"), pos, expr) =>
        compileExpr(pos, expr)
        buf += DupInst
        buf += DupInst
        buf += SLValue.ONE
        buf += (if (op == "++") AddInst else SubInst)
        buf += AssignInst
      case PostfixExpr(pos, expr, op @ ("++" | "--")) =>
        compileExpr(pos, expr)
        buf += DupInst
        buf += DerefInst
        buf += SwapInst
        buf += DupInst
        buf += SLValue.ONE
        buf += (if (op == "++") AddInst else SubInst)
        buf += AssignInst
      case FunctionExpr(params, pos, body) =>
        buf += FunctionInst("*anonymous*", new Compilation {
          compileExpr(pos, body)
          buf += RetInst
        }.code, params map (_.name))
      case VoidExpr => buf += SLVoid
      case CompareExpr(lpos, left, right) =>
        val fixups = new ListBuffer[Int]

        compileExpr(lpos, left)

        right.zipWithIndex foreach {
          case (Predicate(op, pos, expr), idx) =>
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
      case InfixExpr(lpos, left, op, rpos, right) =>
        compileExpr(lpos, left)
        buf += DerefInst
        compileExpr(rpos, right)
        buf += DerefInst
        buf +=
          (op match {
            case "+"   => AddInst
            case "-"   => SubInst
            case "*"   => MulInst
            case "/"   => DivInst
            case "mod" => ModInst
          })
      case AssignExpr(lpos, lvalue, rpos, expr) =>
        lvalue match {
          case SymExpr(Ident(pos, name)) if (decls contains name) && decls(name).isInstanceOf[ValStat] =>
            compileExpr(rpos, expr)
            buf += SLString(name)
            buf += OverInst
            buf += ConstInst
          case _ =>
            compileExpr(rpos, expr)
            compileExpr(lpos, lvalue, lvalue = true)
            buf += OverInst
            buf += AssignInst
        }
      case ApplyExpr(fpos, expr, calls) =>
        def generateCall(as: Args): Unit = {
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
          generateCall(a)
        }
      case WhileExpr(label, pos, cond, body, no) =>
        val start = buf.length

        loops push new Loop
        compileExpr(pos, cond)

        val exit = forward(BranchIfFalseInst)

        compileExpr(NOPOS, body)
        buf += DropInst
        loop(start)
        patch(exit)

        if (no.isDefined)
          compileExpr(NOPOS, no.get)
        else {
          loops.top.patchBreaks()
          buf += SLVoid
        }
      case ConditionalExpr(pos, cond, yes, no) =>
        compileExpr(pos, cond)

        val jumpno = forward(BranchIfFalseInst)

        compileExpr(NOPOS, yes)

        val jumpyes = forward(BranchInst) // if (no.isDefined) forward(BranchInst) else 0

        patch(jumpno)

        no foreach { e =>
          compileExpr(NOPOS, e)
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
      case _: ClassStat =>
      case _: DefStat   =>
      case d @ VarStat(Ident(pos, name), init) =>
        buf += PosInst(pos)
        buf += SLString(name)
        buf += LvalueInst

        if (init.isDefined) {
          compileExpr(NOPOS, init.get)
          buf += AssignInst
        } else
          buf += DropInst

        d.initialized = true
      case v @ ValStat(Ident(pos, name), init, _) =>
        buf += PosInst(pos)
        buf += SLString(name)
        buf += SymInst
        compileExpr(NOPOS, init)
        buf += ConstInst
        v.initialized = true
      case ExpressionStat(expr) => compileExpr(NOPOS, expr)
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

  def apply(module: ModuleAST): Code =
    new Compilation {
      compileDecls(module.stats)
      compileStats(module.stats)
    }.code

}
