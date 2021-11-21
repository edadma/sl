package io.github.edadma.sl

trait Inst {
  def execute(env: Env): Unit
}

case class PosInst(p: SLParser#Position) extends Inst {
  def execute(env: Env): Unit = env pos p
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

case object LteInst extends Inst {
  def execute(env: Env): Unit = env pushb (env.popn.doubleValue >= env.popn.doubleValue)
}

case object LtInst extends Inst {
  def execute(env: Env): Unit = env pushb (env.popn.doubleValue > env.popn.doubleValue)
}

case object EqInst extends Inst {
  def execute(env: Env): Unit = env pushb (env.popn.doubleValue == env.popn.doubleValue)
}

case object DivInst extends Inst {
  def execute(env: Env): Unit = env pushb (env.popn.doubleValue % env.popn.doubleValue == 0)
}

case object MutableInst extends Inst {
  def execute(env: Env): Unit =
    if (!env.top.isInstanceOf[Mutable])
      env.problem("not an l-value")
}

case object AssignInst extends Inst {
  def execute(env: Env): Unit = {
    val newValue = env.pop.deref
    val mutable = env.pop.asInstanceOf[Mutable]

    mutable.value = newValue
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

case object BranchInst extends Inst {
  def execute(env: Env): Unit = env.branch(env.popi)
}

case object CallableInst extends Inst {
  def execute(env: Env): Unit = if (!env.top.isInstanceOf[Callable]) env.problem(s"not callable: ${env.top}")
}

case object CallInst extends Inst {
  def execute(env: Env): Unit = {
    val args = Seq.fill(env.popi.intValue)(env.pop.deref)

    env.pop.deref.asInstanceOf[Callable].call(env, args)
  }
}

case object SymInst extends Inst {
  def execute(env: Env): Unit = env push env.symbol(env.pops)
}
