package org.perl8.test.sbt

import org.scalatools.testing

import org.perl8.test.harness.SummaryReporter
import org.perl8.test.Test

class Runner (
  loader:  ClassLoader,
  loggers: Array[testing.Logger]
) extends testing.Runner2 {
  def run (
    testClassName: String,
    fingerprint:   testing.Fingerprint,
    eventHandler:  testing.EventHandler,
    args:          Array[String]
  ) {
    val reporter = new SBTReporter(loader, loggers, eventHandler)
    reporter.run(testClassName)
  }
}
