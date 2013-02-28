package org.perl8.test.sbt

import java.io.ByteArrayOutputStream
import org.scalatools.testing

import org.perl8.test.harness.Reporter
import org.perl8.test.tap.Parser
import org.perl8.test.Test

class SBTReporter (
  loader:       ClassLoader,
  loggers:      Array[testing.Logger],
  eventHandler: testing.EventHandler
) extends Reporter {
  def run (testName: String): Int = {
    val test = loader.loadClass(testName).newInstance.asInstanceOf[Test]

    val out = new ByteArrayOutputStream
    Console.withOut(out) {
      test.runInHarness
    }

    val result = (new Parser).parse(out)

    result.results.foreach { r =>
      val event = new testing.Event {
        val testName:    String = r.description
        val description: String = r.description
        val result:      testing.Result =
          if (r.passed) {
            testing.Result.Success
          }
          else if (r.directive.isDefined) {
            testing.Result.Skipped
          }
          else {
            testing.Result.Failure
          }
        val error: Throwable = null
      }
      eventHandler.handle(event)
    }

    if (result.success) {
      logInfo(testName + " succeeded.")
      0
    }
    else {
      logError(testName + " failed.")
      1
    }
  }

  private def logDebug (msg: String) {
    loggers.foreach(_.debug(msg))
  }

  private def logInfo (msg: String) {
    loggers.foreach(_.info(msg))
  }

  private def logWarn (msg: String) {
    loggers.foreach(_.warn(msg))
  }

  private def logError (msg: String) {
    loggers.foreach(_.error(msg))
  }
}
