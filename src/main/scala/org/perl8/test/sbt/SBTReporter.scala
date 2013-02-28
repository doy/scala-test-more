package org.perl8.test.sbt

import java.io.ByteArrayOutputStream
import org.scalatools.testing

import org.perl8.test.harness.{Reporter,SummarizedTests}
import org.perl8.test.Test

class SBTReporter (
  loader:       ClassLoader,
  loggers:      Array[testing.Logger],
  eventHandler: testing.EventHandler
) extends Reporter with SummarizedTests {
  def run (testName: String): Int = {
    val test = loader.loadClass(testName).newInstance.asInstanceOf[Test]
    val result = runOneTest(test, e => ())

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
      logInfo("PASS " + testName)
      0
    }
    else {
      logError("FAIL " + testName)
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
