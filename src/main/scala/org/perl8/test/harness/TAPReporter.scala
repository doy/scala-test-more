package org.perl8.test.harness

import org.perl8.test.Test
import Utils._

class TAPReporter extends Reporter {
  def run (testName: String): Int =
    newInstance[Test](testName).run
}
