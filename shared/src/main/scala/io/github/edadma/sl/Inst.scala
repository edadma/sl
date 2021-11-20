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

case object MutableInst extends Inst {
  def execute(env: Env): Unit =
    if (!env.top.isInstanceOf[Mutable])
      env.problem("not an l-value")
}

case object AssignInst extends Inst {
  def execute(env: Env): Unit = {
    val newValue = env.pop
    val mutable = env.pop.asInstanceOf[Mutable]

    mutable.value = newValue
  }
}

case object CallInst extends Inst {
  def execute(env: Env): Unit =
    env.pop match {
      case c: Callable => c.call(env, Seq.fill(env.popn.intValue)(env.pop))
      case x           => env.problem(s"not callable: $x")
    }
}
