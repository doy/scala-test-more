package org.perl8.test.sbt

import org.scalatools.testing.{EventHandler,Logger}

import org.perl8.test.harness.Utils._
import org.perl8.test.harness.SummaryReporter
import org.perl8.test.Test

class Runner (
  loader:  ClassLoader,
  loggers: Array[Logger]
) extends org.scalatools.testing.Runner2 {
  def run (
    testClassName: String,
    fingerprint:   org.scalatools.testing.Fingerprint,
    eventHandler:  EventHandler,
    args:          Array[String]
  ) {
    val reporter = new SBTReporter(loader, loggers, eventHandler)
    reporter.run(testClassName)
  }
}
