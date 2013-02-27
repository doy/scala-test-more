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

  case class NumericPlan (
    override val plan: Int
  ) extends Plan {
    override val skipAll = false
    override val message = None
  }

  case class SkipAll (
    override val message: Option[String] = None
  ) extends Plan {
    override val plan    = 0
    override val skipAll = true
  }

  object SkipAll {
    def apply (reason: String) = new SkipAll(Some(reason))
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
