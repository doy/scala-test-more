package org.perl8.test.harness

import java.io.OutputStream

import org.perl8.test.Test
import Utils._

trait Reporter {
  def run (test: Test): Int

  def run (testName: String): Int =
    run(newInstance[Test](testName))
}
