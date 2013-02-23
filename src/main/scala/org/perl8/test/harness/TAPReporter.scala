package org.perl8.test.harness

import java.io.ByteArrayOutputStream

import org.perl8.test.tap
import org.perl8.test.Test
import Utils._

class TAPReporter extends Reporter {
  def run (testName: String): Int = {
    val out = new ByteArrayOutputStream
    Console.withOut(out) {
      Console.withErr(out) {
        newInstance[Test](testName).run
      }
    }

    // XXX this is wrong: it sends everything to stdout
    // need to write a tee-like outputstream to fix it
    print(out)

    val result = tap.Consumer.parse(out)
    result.exitCode
  }
}
