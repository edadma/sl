package io.github.edadma.sl

import scala.collection.mutable.ArrayBuffer

object Compiler {

  def compileBlock(stats: Seq[StatAST]): CodeBlock = {
    val buf = new ArrayBuffer[Inst]

    stats foreach {
      case DefStat(ident, params, body) =>
      case VarStat(ident, init)         =>
      case ExpressionStat(expr)         =>
    }

    new CodeBlock(buf)
  }

  def apply(ast: SLAST): CodeBlock =
    ast match {
      case SourcesAST(stats) => compileBlock(stats)
      case BlockExpr(stats)  => compileBlock(stats)
    }

}
