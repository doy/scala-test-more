package org.perl8.test.harness

trait MultiTestReporter {
  def run (testNames: Seq[String]): Int
}
