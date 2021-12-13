package io.github.edadma.sl

import io.github.edadma.dal.ComplexDAL

trait Inst {
  def execute(env: Env): Unit
}

case class PosInst(p: Int) extends Inst {
  def execute(env: Env): Unit = env pos p
}

case object NegInst extends Inst {
  def execute(env: Env): Unit = env push ComplexDAL.negate(env.popn, SLNumber.from)
}

case object NotInst extends Inst {
  def execute(env: Env): Unit = env pushb !env.popb
}

case object AddInst extends Inst {
  val op: Symbol = Symbol("+")

  def execute(env: Env): Unit = env push ComplexDAL.compute(op, env.popn, env.popn, SLNumber.from)
}

case object PowInst extends Inst {
  val op: Symbol = Symbol("^")

  def execute(env: Env): Unit = {
    val power = env.popn

    env push ComplexDAL.compute(op, env.popn, power, SLNumber.from)
  }
}

case object RangeInst extends Inst {
  def execute(env: Env): Unit = {
    val end = env.popi
    val start = env.popi

    env push SLRange(start, end)
  }
}

case object SubInst extends Inst {
  val op: Symbol = Symbol("-")

  def execute(env: Env): Unit = {
    val subtrahend = env.popn

    env push ComplexDAL.compute(op, env.popn, subtrahend, SLNumber.from)
  }
}

case object MulInst extends Inst {
  val op: Symbol = Symbol("*")

  def execute(env: Env): Unit = env push ComplexDAL.compute(op, env.popn, env.popn, SLNumber.from)
}

case object DivInst extends Inst {
  val op: Symbol = Symbol("/")

  def execute(env: Env): Unit = {
    val divisor = env.popn

    env push ComplexDAL.compute(op, env.popn, divisor, SLNumber.from)
  }
}

case object LteInst extends Inst {
  val op: Symbol = Symbol(">=")

  def execute(env: Env): Unit = env pushb ComplexDAL.relate(op, env.popn, env.popn)
}

case object LtInst extends Inst {
  val op: Symbol = Symbol(">")

  def execute(env: Env): Unit = env pushb ComplexDAL.relate(op, env.popn, env.popn)
}

case object EqInst extends Inst {
  val op: Symbol = Symbol("=")

  def execute(env: Env): Unit = {
    val a = env.pop.deref
    val b = env.pop.deref

    env pushb (if (a.clas == b.clas)
                 ((a, b) match {
                   case (x: SLNumber, y: SLNumber) => ComplexDAL.relate(op, x, y)
                   case (x, y)                     => x == y
                 })
               else
                 false)
  }
}

case object GteInst extends Inst {
  val op: Symbol = Symbol("<=")

  def execute(env: Env): Unit = env pushb ComplexDAL.relate(op, env.popn, env.popn)
}

case object GtInst extends Inst {
  val op: Symbol = Symbol("<")

  def execute(env: Env): Unit = env pushb ComplexDAL.relate(op, env.popn, env.popn)
}

case object NeInst extends Inst {
  def execute(env: Env): Unit = env pushb (env.pop.deref != env.pop.deref)
}

case object ModInst extends Inst {
  def execute(env: Env): Unit = {
    val divisor = env.popn
    val OP: Symbol = Symbol("mod")

    env push ComplexDAL.compute(OP, env.popn, divisor, SLNumber.from)
  }
}

case object AssignInst extends Inst {
  def execute(env: Env): Unit = {
    val newValue = env.pop.deref

    env.pop match {
      case m: Mutable => m.value = newValue
      case x          => env.problem(s"not an l-value: $x")
    }
  }
}

case object ConstInst extends Inst {
  def execute(env: Env): Unit = {
    val value = env.pop.deref
    val name = env.pops

    env.act.define(name, value)
  }
}

case object BranchIfFalseInst extends Inst {
  def execute(env: Env): Unit = {
    val disp = env.popi
    val cond = env.popb

    if (!cond)
      env.branch(disp)
  }
}

case object BranchIfTrueInst extends Inst {
  def execute(env: Env): Unit = {
    val disp = env.popi
    val cond = env.popb

    if (cond)
      env.branch(disp)
  }
}

case object BranchIfTrueBoolInst extends Inst {
  def execute(env: Env): Unit = {
    val disp = env.popi
    val cond = env.topb

    if (cond)
      env.branch(disp)
    else
      env.pop
  }
}

case object BranchIfFalseBoolInst extends Inst {
  def execute(env: Env): Unit = {
    val disp = env.popi
    val cond = env.topb

    if (cond)
      env.pop
    else
      env.branch(disp)
  }
}

case object BranchIfFalseCompareInst extends Inst {
  def execute(env: Env): Unit = {
    val disp = env.popi
    val cond = env.popb

    if (!cond) {
      env.pop
      env push SLValue.FALSE
      env.branch(disp)
    }
  }
}

//case object BranchIfTrueCompareInst extends Inst {
//  def execute(env: Env): Unit = {
//    val disp = env.popi
//    val cond = env.popb
//
//    if (cond) {
//      env.pop
//      env push SLValue.TRUE
//      env.branch(disp)
//    }
//  }
//}

case object BranchInst extends Inst {
  def execute(env: Env): Unit = env.branch(env.popi)
}

case object DupInst extends Inst {
  def execute(env: Env): Unit = env push env.top
}

case object SwapInst extends Inst {
  def execute(env: Env): Unit = {
    val a = env.pop
    val b = env.pop

    env push a
    env push b
  }
}

case object OverInst extends Inst {
  def execute(env: Env): Unit = env push env(1)
}

case object DropInst extends Inst {
  def execute(env: Env): Unit = env.pop
}

case object InstanceInst extends Inst {
  def execute(env: Env): Unit = env push Instance(env.act.asInstanceOf[ConstructorActivation])
}

case class FunctionInst(name: String, code: Code, parms: Seq[String]) extends Inst {
  def execute(env: Env): Unit = env push DefinedFunction(name, code, parms, env.act)
}

case class ClassInst(name: String, code: Code, parms: Seq[String]) extends Inst {
  def execute(env: Env): Unit = env push DefinedClass(name, code, parms, env.act)
}

case object DotInst extends Inst {
  def execute(env: Env): Unit = {
    val elem = env.pops

    env.pop.deref match {
      case Instance(con) => env push (con.locals getOrElse (elem, SLUndefined))
      case SLMap(m)      => env push (m getOrElse (SLString(elem), SLUndefined))
    }
  }
}

case object DerefInst extends Inst {
  def execute(env: Env): Unit = env push env.pop.deref
}

case object CallInst extends Inst {
  def execute(env: Env): Unit = {
    val args = Seq.fill(env.popi.intValue)(env.pop.deref).reverse // todo: not efficient

    env.pop.deref match {
      case c: Callable => c.call(env, args)
      case x           => if (!env.top.deref.isInstanceOf[Callable]) env.problem(s"not callable: $x")
    }
  }
}

case object IterInst extends Inst {
  def execute(env: Env): Unit = env push SLIterator(env.pop.deref.asInstanceOf[SLIterable].iterator)
}

case object HasNextInst extends Inst {
  def execute(env: Env): Unit = env pushb env.pop.deref.asInstanceOf[SLIterator].it.hasNext
}

case object NextInst extends Inst {
  def execute(env: Env): Unit = env push env.pop.deref.asInstanceOf[SLIterator].it.next()
}

case object SymInst extends Inst {
  def execute(env: Env): Unit = env push env.symbol(env.pops)
}

case object VarInst extends Inst {
  def execute(env: Env): Unit = {
    val value = env.pop.deref
    val name = env.pops

    env push env.act.define(name, new VarMutable(value))
  }
}

case object RetInst extends Inst {
  def execute(env: Env): Unit = env.act = env.act.asInstanceOf[FunctionLikeActivation].caller
}

case object ListPrependInst extends Inst {
  def execute(env: Env): Unit = {
    val item = env.pop.deref
    val list = env.pop.deref.asInstanceOf[SLList]

    env push SLList(item :: list.l)
  }
}

case object MapInsertInst extends Inst {
  def execute(env: Env): Unit = {
    val value = env.pop.deref
    val key = env.pop.deref
    val map = env.pop.deref.asInstanceOf[SLMap]

    env push SLMap(map.m.updated(key, value))
  }
}

case object StringBuilderInst extends Inst {
  def execute(env: Env): Unit = env push new SLStringBuilder
}

case object AppendInst extends Inst {
  def execute(env: Env): Unit = {
    val s = env.pops
    val builder = env.top.asInstanceOf[SLStringBuilder].builder

    builder ++= s
  }
}

case object StringFromBuilderInst extends Inst {
  def execute(env: Env): Unit = {
    val builder = env.pop.asInstanceOf[SLStringBuilder].builder

    env push SLString(builder.toString)
  }
}
