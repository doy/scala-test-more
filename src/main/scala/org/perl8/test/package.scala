package org.perl8

package object test {
  import language.implicitConversions

  implicit def intToPlan (p: Int): Plan =
    NumericPlan(p)

  sealed trait Plan {
    val plan:    Int
    val skipAll: Boolean
    val message: Option[String]
  }

  case class NumericPlan (plan: Int) extends Plan {
    val skipAll = false
    val message = None
  }

  case object NoPlan extends Plan {
    val plan    = 0
    val skipAll = false
    val message = None
  }

  case class SkipAll (msg: String) extends Plan {
    val plan    = 0
    val skipAll = true
    val message = Some(msg)
  }

  case object SkipAllNoMessage extends Plan {
    val plan    = 0
    val skipAll = true
    val message = None
  }

  object SkipAll {
    def apply () = SkipAllNoMessage
  }

  case class BailOutException (
    message: String
  ) extends RuntimeException(message)
}
