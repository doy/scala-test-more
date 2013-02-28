package org.perl8

package object test {
  import language.implicitConversions

  implicit def intToPlan (p: Int): Plan =
    NumericPlan(p)

  sealed trait Plan {
    val plan:    Int
    val skipAll: Option[String]
  }

  case class NumericPlan (plan: Int) extends Plan {
    val skipAll = None
  }

  case object NoPlan extends Plan {
    val plan    = 0
    val skipAll = None
  }

  case class SkipAll (message: String) extends Plan {
    val plan    = 0
    val skipAll = Some(message)
  }

  case class BailOutException (
    message: String
  ) extends RuntimeException(message)
}
