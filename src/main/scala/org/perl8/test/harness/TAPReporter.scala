package org.perl8.test.harness

import org.perl8.test.tap
import org.perl8.test.Test

/** Runs a single test. The TAP stream from that test is written directly to
  * stdout/stderr.
  */
class TAPReporter extends Reporter {
  def run (testName: String): Int =
    newInstance[Test](testName).run
}
