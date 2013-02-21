package org.perl8.test.harness

import org.perl8.test.Test
import Utils._

trait MultiTestReporter extends Reporter {
  def run (tests: Array[Test]): Int =
    tests.map(run).sum min 255

  def run (testNames: Array[String]): Int =
    run(testNames.map(newInstance[Test]))
}
