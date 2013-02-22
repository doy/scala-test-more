package org.perl8.test.harness

trait Reporter {
  def run (testName: String): Int
}
