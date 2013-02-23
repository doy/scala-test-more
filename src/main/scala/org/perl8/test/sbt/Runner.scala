package org.perl8.test.sbt

import org.scalatools.testing.{EventHandler,Event,Result}
import java.io.{OutputStream,ByteArrayOutputStream}

import org.perl8.test.harness.Utils._
import org.perl8.test.harness.SummaryReporter
import org.perl8.test.Test

class Runner (
  loader:  ClassLoader,
  loggers: Array[org.scalatools.testing.Logger]
) extends org.scalatools.testing.Runner2 {
  def run (
    testClassName: String,
    fingerprint:   org.scalatools.testing.Fingerprint,
    eventHandler:  EventHandler,
    args:          Array[String]
  ) {
    val reporter = new SummaryReporter
    val results = reporter.runTests(Seq(testClassName))
    results(testClassName)._2.results.foreach { r =>
      val event = new Event {
        val testName: String    = testClassName
        val description: String = r.description
        val result: Result      =
          if (r.passed) {
            org.scalatools.testing.Result.Success
          }
          else if (r.directive.isDefined) {
            org.scalatools.testing.Result.Skipped
          }
          else {
            org.scalatools.testing.Result.Failure
          }
        val error: Throwable = null
      }
      eventHandler.handle(event)
    }
  }

  def println (thing: Any) {
    loggers.foreach(_.info(thing.toString))
  }
}
