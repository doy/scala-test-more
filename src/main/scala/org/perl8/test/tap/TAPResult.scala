package org.perl8.test.tap

import org.perl8.test.{Plan,NumericPlan,SkipAll}

sealed trait Directive {
  val message: Option[String]
}
case class SkipDirective (message: Option[String]) extends Directive
case class TodoDirective (message: Option[String]) extends Directive

class TestResult (
  val passed:      Boolean,
  val number:      Int,
  val description: String,
  val directive:   Option[Directive],
  val subtest:     Option[TAPResult]
)

class TAPResult (val plan: Plan, val results: Seq[TestResult]) {
  val matchesPlan = plan match {
    case NumericPlan(n) => results.length == n
    case _              => results.length == 0
  }

  val fails = results.count { r =>
    !r.passed && !r.directive.isDefined
  }

  val testsPassed = fails == 0

  val success = plan match {
    case SkipAll(_) => true
    case _          => results.length > 0 && fails == 0 && matchesPlan
  }

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
