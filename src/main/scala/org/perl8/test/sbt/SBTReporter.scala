package org.perl8.test.sbt

import org.scalatools.testing

import org.perl8.test.harness.{Reporter,SummarizedTests}
import org.perl8.test.tap.{TAPEvent,ResultEvent,EndEvent}
import org.perl8.test.Test

class SBTReporter (
  loader:       ClassLoader,
  loggers:      Array[testing.Logger],
  eventHandler: testing.EventHandler
) extends Reporter with SummarizedTests {
  def run (testName: String): Int = {
    val cb = (e: TAPEvent) => e match {
      case ResultEvent(r) => {
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
      case EndEvent(result) => {
        if (result.success) {
          logInfo("PASS " + testName)
        }
        else {
          val results = result.results.length
          val failed = result.results.count { t =>
            !t.passed && !t.directive.isDefined
          }

          logError(
            "FAIL " + testName + " " +
            "(failed " + failed + "/" + results + ")"
          )
        }
      }
      case _ => ()
    }

    runOneTest(
      loader.loadClass(testName).newInstance.asInstanceOf[Test],
      cb
    ).exitCode
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
