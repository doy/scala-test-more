package org.perl8

package object test {
  import language.implicitConversions

  /** Converts an [[scala.Int Int]] to a [[NumericPlan]].
    */
  implicit def intToPlan (p: Int): Plan =
    NumericPlan(p)

  /** A test plan. This represents the TAP statement telling how many tests
    * will be run.
    */
  sealed trait Plan {
    /** How many tests will be run.
      */
    val plan:    Int
    /** Whether this test was skipped. It should contain `Some(message)` if
      * the test is skipped, and `None` otherwise.
      */
    val skipAll: Option[String]
  }

  /** An explicit plan number. Corresponds to `1..5` in TAP.
    */
  case class NumericPlan (plan: Int) extends Plan {
    /** @inheritdoc
      *
      * Always `None` for this class.
      */
    val skipAll = None
  }

  /** A test which did not declare a plan yet.
    */
  case object NoPlan extends Plan {
    /** @inheritdoc
      *
      * Always 0 for this class.
      */
    val plan    = 0
    /** @inheritdoc
      *
      * Always `None` for this class.
      */
    val skipAll = None
  }

  /** A test which has declared that the entire test has been skipped.
    * Corresponds to `1..0 # SKIP [message]` in TAP.
    */
  case class SkipAll (message: String) extends Plan {
    /** @inheritdoc
      *
      * Always 0 for this class.
      */
    val plan    = 0
    /** @inheritdoc
      *
      * Never `None` for this class.
      */
    val skipAll = Some(message)
  }

  /** Exception thrown when a test bails out. Corresponds to
    * `Bail out!  [message]` in TAP.
    */
  case class BailOutException (
    message: String
  ) extends RuntimeException(message)
}
