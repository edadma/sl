package io.github.edadma.sl

trait Inst {
  def execute(env: Env): Unit
}

case class Pos(p: SLParser#Position) extends Inst {
  def execute(env: Env): Unit = env pos p
}

case class Push(v: SLValue) extends Inst {
  def execute(env: Env): Unit = env push v
}

case object Add extends Inst {
  def execute(env: Env): Unit = env pushn (env.popn.doubleValue + env.popn.doubleValue)
}

case object Sub extends Inst {
  def execute(env: Env): Unit = {
    val subtrahend = env.popn.doubleValue

    env pushn (env.popn.doubleValue - subtrahend)
  }
}
