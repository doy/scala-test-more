package org.perl8.test.sbt

import org.scalatools.testing.{EventHandler,Event,Result,Logger}
import java.io.ByteArrayOutputStream

import org.perl8.test.harness._
import org.perl8.test.tap
import org.perl8.test.Test

class SBTReporter (
  loader:       ClassLoader,
  loggers:      Array[Logger],
  eventHandler: EventHandler
) extends Reporter {
  def run (testName: String): Int = {
    val test = loader.loadClass(testName).newInstance.asInstanceOf[Test]

    val out = new ByteArrayOutputStream
    Console.withOut(out) {
      test.run
    }

    val result = tap.Consumer.parse(out)

    result.results.foreach { r =>
      val event = new Event {
        val testName:    String = r.description
        val description: String = r.description
        val result:      Result =
          if (r.passed) {
            Result.Success
          }
          else if (r.directive.isDefined) {
            Result.Skipped
          }
          else {
            Result.Failure
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
