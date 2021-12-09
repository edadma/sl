package io.github.edadma.sl

trait Inst {
  def execute(env: Env): Unit
}

case class PosInst(p: Int) extends Inst {
  def execute(env: Env): Unit = env pos p
}

case object NegInst extends Inst {
  def execute(env: Env): Unit = env pushn -env.popn.doubleValue
}

case object NotInst extends Inst {
  def execute(env: Env): Unit = env pushb !env.popb
}

case object AddInst extends Inst {
  def execute(env: Env): Unit = env pushn (env.popn.doubleValue + env.popn.doubleValue)
}

case object SubInst extends Inst {
  def execute(env: Env): Unit = {
    val subtrahend = env.popn.doubleValue

    env pushn (env.popn.doubleValue - subtrahend)
  }
}

case object MulInst extends Inst {
  def execute(env: Env): Unit = env pushn (env.popn.doubleValue * env.popn.doubleValue)
}

case object DivInst extends Inst {
  def execute(env: Env): Unit = {
    val divisor = env.popn.doubleValue

    env pushn (env.popn.doubleValue / divisor)
  }
}

case object LteInst extends Inst {
  def execute(env: Env): Unit = env pushb (env.popn.doubleValue >= env.popn.doubleValue)
}

case object LtInst extends Inst {
  def execute(env: Env): Unit = env pushb (env.popn.doubleValue > env.popn.doubleValue)
}

case object EqInst extends Inst {
  def execute(env: Env): Unit = env pushb (env.pop.deref == env.pop.deref)
}

case object GteInst extends Inst {
  def execute(env: Env): Unit = env pushb (env.popn.doubleValue <= env.popn.doubleValue)
}

case object GtInst extends Inst {
  def execute(env: Env): Unit = env pushb (env.popn.doubleValue < env.popn.doubleValue)
}

case object NeInst extends Inst {
  def execute(env: Env): Unit = env pushb (env.pop.deref != env.pop.deref)
}

case object ModInst extends Inst {
  def execute(env: Env): Unit = {
    val divisor = env.popn.doubleValue

    env pushn (env.popn.doubleValue % divisor)
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

case object SymInst extends Inst {
  def execute(env: Env): Unit = env push env.symbol(env.pops)
}

case object LvalueInst extends Inst {
  def execute(env: Env): Unit = env push env.lvalue(env.pops)
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
