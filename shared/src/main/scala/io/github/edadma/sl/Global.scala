package io.github.edadma.sl

import io.github.edadma.dal.ComplexBigIntType
import io.github.edadma.numbers.ComplexBigInt

import scala.language.postfixOps

object Global {

  val map: Map[String, SLValue] =
    List(
      "println" -> SLNativeFunction("println", args => {
        println(args mkString ", ")
        SLVoid
      }),
      "i" -> SLNumber(ComplexBigIntType, ComplexBigInt.i)
    ) toMap

}
