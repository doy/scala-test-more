package org.perl8.test.harness

import org.perl8.test.Test
import Utils._

object TestHarness {
  def main (args: Array[String]) {
    val (reporterName, idx, multi) = if (args.length >= 2 && args(0) == "-r") {
      (args(1), 2, false)
    }
    else if (args.length >= 2 && args(0) == "-R") {
      (args(1), 2, true)
    }
    else if (args.length > 1) {
      ("org.perl8.test.harness.SummaryReporter", 0, true)
    }
    else if (args.length == 1) {
      ("org.perl8.test.harness.TAPReporter", 0, false)
    }
    else {
      println("No tests specified!")
      sys.exit(1)
    }

    val exitCode = if (multi) {
      val testNames = args.drop(idx)
      val reporter = newInstance[MultiTestReporter](reporterName)
      reporter.run(testNames)
    }
    else {
      val testName = args(idx)
      val reporter = newInstance[Reporter](reporterName)
      reporter.run(testName)
    }

    sys.exit(exitCode)
  }
}
