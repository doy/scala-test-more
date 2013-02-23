package org.perl8.test.harness

import java.io.OutputStream

import org.perl8.test.Test
import Utils._

class TAPReporter extends Reporter {
  def run (testName: String): Int =
    newInstance[Test, OutputStream](testName, System.out).run
}
