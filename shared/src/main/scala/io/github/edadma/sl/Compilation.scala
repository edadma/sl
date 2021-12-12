package io.github.edadma.sl

import io.github.edadma.dal.{ComplexDAL, DoubleType}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class Compilation {
  private val NOPOS = -1

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

  def patch(fixup: Int): Unit = buf(fixup) = SLNumber.from(buf.length - fixup - 2)

  def loop(start: Int, inst: Inst = BranchInst): Unit = {
    buf += SLNumber.from(-(buf.length - start) - 2)
    buf += inst
  }

  def code: Code = new Code(buf)

  def compileDecls(expr: ExprAST): Unit =
    expr match {
      case BlockExpr(stats) => compileDecls(stats)
      case _                =>
    }

  def compileDecls(stats: Seq[StatAST]): Unit = {
    def duplicate(pos: Int, name: String): Unit =
      if (decls contains name)
        problem(pos, s"duplicate declaration: $name")

    stats foreach {
      case d @ DefStat(Ident(pos, name), params, body) =>
        duplicate(pos, name)
        buf += SLString(name)
        buf += FunctionInst(name, new Compilation {
          compileDecls(body)
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
      case ExpressionStat(f @ ForExpr(_, Ident(pos, name), _, _, _, _)) =>
        decls get name match {
          case None =>
            decls(name) = ValStat(Ident(pos, name), null)

            val it = s"$name#${decls.size}"

            decls(it) = ValStat(Ident(pos, it), null)
            f.it = it
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
      case ContinueExpr(pos, label) =>
        if (loops.isEmpty)
          problem(pos, "'continue' not inside a loop construct")

        loop(loops.top.start)
      case NullExpr => buf += SLNull
      case BreakExpr(pos, label, expr) =>
        if (loops.isEmpty)
          problem(pos, "'break' not inside a loop construct")

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
      case SeqExpr(elems) =>
        buf += SLValue.NIL

        for (e <- elems.reverse) {
          compileExpr(NOPOS, e)
          buf += ListPrependInst
        }
      case BooleanExpr(b) => buf += SLBoolean(b == "true")
      case PrefixExpr("not", pos, expr) =>
        compileExpr(pos, expr)
        buf += NotInst
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
          compileDecls(body)
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
              case ">="  => buf += GteInst
              case ">"   => buf += GtInst
              case "!="  => buf += NeInst
              case "div" => buf ++= Seq(SwapInst, ModInst, SLValue.ZERO, EqInst)
            }

            if (!last) {
              fixups += forward(BranchIfFalseCompareInst)
            }
        }

        fixups foreach patch
      case BlockExpr(stats) => compileBlock(stats)
      case SymExpr(ident)   => buf ++= Seq(PosInst(ident.pos), SLString(ident.name), if (lvalue) LvalueInst else SymInst)
      case NumberExpr(n) =>
        val (t, v) =
          if (n.contains('.') || n.contains('e') || n.contains('E'))
            (DoubleType, n.toDouble.asInstanceOf[Number])
          else
            ComplexDAL.maybeDemote(BigInt(n))

        buf += SLNumber(t, v)
      case StringExpr(s) =>
        val str = new StringBuilder
        var i = 0

        while (i < s.length) s.charAt(i) match {
          case '\\' =>
            str +=
              (s.charAt(i + 1) match {
                case '\'' => '\''
                case '"'  => '"'
                case '/'  => '/'
                case '\\' => '\\'
                case 'b'  => '\b'
                case 'f'  => '\f'
                case 'n'  => '\n'
                case 'r'  => '\r'
                case 't'  => '\t'
              })
            i += 2
          case c =>
            str += c
            i += 1
        }

        buf += SLString(str.toString)
      case InfixExpr(lpos, left, op @ ("or" | "and"), rpos, right) =>
        compileExpr(lpos, left)
        buf += DerefInst

        val fixup = forward(if (op == "or") BranchIfTrueBoolInst else BranchIfFalseBoolInst)

        compileExpr(rpos, right)
        buf += DerefInst
        patch(fixup)
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
            case "^"   => PowInst
            case ".."  => RangeInst
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
        compileExpr(fpos, expr)

        calls foreach {
          case Args(args) =>
            args foreach {
              case Arg(pos, expr) =>
                compileExpr(pos, expr)
                buf += DerefInst
            }

            buf += SLNumber.from(args.length)
            buf += CallInst
          case Dot(Ident(epos, elem)) =>
            buf += PosInst(epos)
            buf += SLString(elem)
            buf += DotInst
        }
      case f @ ForExpr(label, index, pos, iterable, body, no) =>
        buf += SLString(f.it)
        compileExpr(pos, iterable)
        buf += IterInst
        buf += ConstInst

        val start = buf.length

        loops push new Loop
        buf += SLString(f.it)
        buf += SymInst
        buf += HasNextInst

        val exit = forward(BranchIfFalseInst)

        buf += SLString(index.name)
        buf += SLString(f.it)
        buf += SymInst
        buf += NextInst
        buf += ConstInst
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
      case DoWhileExpr(label, body, pos, cond, no) =>
        val start = buf.length

        loops push new Loop

        compileExpr(NOPOS, body)
        buf += DropInst
        compileExpr(pos, cond)
        loop(start, BranchIfTrueInst)

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
