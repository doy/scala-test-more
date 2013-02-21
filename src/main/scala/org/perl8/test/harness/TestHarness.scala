package org.perl8.test.harness

import org.perl8.test.Test
import Utils._

object TestHarness {
  def main (args: Array[String]) {
    val reporterName = args(0)
    val testName = args(1)
    val reporter = newInstance[Reporter](reporterName)
    val exitCode = reporter.run(testName)
    sys.exit(exitCode)
  }
}
