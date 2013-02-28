package org.perl8

package object test {
  import language.implicitConversions

  implicit def intToPlan (p: Int): Plan =
    NumericPlan(p)

  implicit def stringToMessage (s: String): Message =
    new HasMessage(s)

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

  sealed trait Message
  case class HasMessage (val contents: String) extends Message
  case object NoMessage extends Message

  implicit def messageToOption (message: Message): Option[String] =
    message match {
      case HasMessage(x) => Some(x)
      case NoMessage     => None
    }

  implicit def optionToMessage (option: Option[String]): Message =
    option match {
      case Some(x) => HasMessage(x)
      case None    => NoMessage
    }

  case class BailOutException (val message: String)
    extends RuntimeException(message)
}
