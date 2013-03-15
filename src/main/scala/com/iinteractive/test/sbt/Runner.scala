package com.iinteractive.test.sbt

import org.scalatools.testing

import com.iinteractive.test.harness.SummaryReporter
import com.iinteractive.test.Test

/** Implementation of
  * [[http://github.com/harrah/test-interface/blob/master/src/org/scalatools/testing/Runner2.java org.scalatools.testing.Runner2]]
  * using [[com.iinteractive.test.sbt.SBTReporter SBTReporter]].
  */
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
    val verbose = args.contains("-v")
    val reporter = new SBTReporter(loader, loggers, eventHandler, verbose)
    reporter.run(testClassName)
  }
}
