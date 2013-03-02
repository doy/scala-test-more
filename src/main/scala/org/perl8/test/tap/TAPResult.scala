package org.perl8.test.tap

import org.perl8.test.{Plan,NumericPlan,SkipAll}

/** The summarized results of a TAP stream. Contains the
  * [[org.perl8.test.Plan Plan]] that was given, as well as a list of
  * [[org.perl8.test.tap.TestResult TestResults]] corresponding to each of the
  * tests in the stream.
  *
  * @param plan    The [[org.perl8.test.Plan Plan]] from the TAP stream
  * @param results The list of [[org.perl8.test.tap.TestResult TestResults]]
  *                from the TAP stream
  */
class TAPResult (val plan: Plan, val results: Seq[TestResult]) {
  /** Returns true if the number of tests executed was compatible with the
    * provided test plan.
    */
  val matchesPlan = plan match {
    case NumericPlan(n) => results.length == n
    case _              => results.length == 0
  }

  /** Returns the number of test failures in the TAP stream. */
  val fails = results.count { r =>
    !r.passed && !r.directive.isDefined
  }

  /** Returns true if all of the tests passed. */
  val testsPassed = fails == 0

  /** Returns true if the TAP stream overall passed.
    *
    * Differs from `testsPassed` in that it also takes into account things
    * like invalid plans.
    */
  val success = plan match {
    case SkipAll(_) => true
    case _          => results.length > 0 && fails == 0 && matchesPlan
  }

  /** Returns the exit code to use if running this test on its own.
    *
    * Success is indicated by 0, invalid TAP streams (such as incorrect plans)
    * by 255, and other kinds of failures by the failure count.
    */
  val exitCode =
    if (success) {
      0
    }
    else if (!matchesPlan || results.length == 0) {
      255
    }
    else {
      fails
    }
}

/** The result of a single test.
  *
  * @param passed      True if the test passed
  * @param number      The test number in the TAP stream
  * @param description The test description
  * @param directive   The [[org.perl8.test.tap.Directive Directive]] (either
  *                    skip or todo) that was provided for this test, if any
  * @param subtest     The [[org.perl8.test.tap.TAPResult]] for the subtest
  *                    that this test corresponds to, if any
  */
class TestResult (
  val passed:      Boolean,
  val number:      Int,
  val description: String,
  val directive:   Option[Directive],
  val subtest:     Option[TAPResult]
)

/** A modifier associated with a test result. This is indicated by a `#` at
  * the end of the result line, followed by the type of directive, and an
  * optional message.
  */
sealed trait Directive {
  val message: Option[String]
}

/** A directive indicating that this test was skipped. */
case class SkipDirective private[tap] (
  message: Option[String]
) extends Directive

/** A directive indicating that this test is known to fail. */
case class TodoDirective private[tap] (
  message: Option[String]
) extends Directive
