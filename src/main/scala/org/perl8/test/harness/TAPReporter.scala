package org.perl8.test.harness

import org.perl8.test.Test

class TAPReporter extends Reporter {
  def run (test: Test): Int =
    test.run
}
