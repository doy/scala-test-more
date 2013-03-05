package com.iinteractive.test.harness

import com.iinteractive.test.tap
import com.iinteractive.test.Test

/** Runs a single test. The TAP stream from that test is written directly to
  * stdout/stderr.
  */
class TAPReporter extends Reporter {
  def run (testName: String): Int =
    newInstance[Test](testName).run
}
