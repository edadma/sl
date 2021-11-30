package io.github.edadma.sl

import scala.language.postfixOps

object Global {

  val map: Map[String, SLNativeFunction] =
    List(
      SLNativeFunction("println", args => {
        println(args mkString ", ")
        SLVoid
      })
    ) map (n => n.name -> n) toMap

}
