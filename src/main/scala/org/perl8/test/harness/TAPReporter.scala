package org.perl8.test.harness

import java.io.ByteArrayOutputStream

import org.perl8.test.tap
import org.perl8.test.Test

class TAPReporter extends Reporter {
  def run (testName: String): Int =
    newInstance[Test](testName).runRaw
}
