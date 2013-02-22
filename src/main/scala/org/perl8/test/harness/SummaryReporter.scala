package org.perl8.test.harness

import java.io.{OutputStream,ByteArrayOutputStream}

import org.perl8.test.Test
import org.perl8.test.tap
import Utils._

class SummaryReporter extends MultiTestReporter {
  def run (testNames: Seq[String]): Int = {
    val maxLength = testNames.map(_.length).max
    val (exits, results) = testNames.map { name =>
      print(name + ("." * (maxLength - name.length)) + "... ")
      val out = new ByteArrayOutputStream
      val test = newInstance[Test, OutputStream](name, out)
      val exitCode = test.run
      val result = tap.Consumer.parse(out)
      if (exitCode == 0) {
        println("ok")
      }
      else {
        val results = result.results
        val failed = results.filter(t => !t.passed && !t.directive.isDefined)
        println("Dubious, test returned " + exitCode)
        println("Failed " + failed.length + "/" + results.length + " subtests")
      }
      (exitCode, result)
    }.unzip

    val exitMap   = (testNames zip exits).toMap
    val resultMap = (testNames zip results).toMap

    val exitCode = if (exits.filter(_ != 0).nonEmpty) { 1 } else { 0 }

    if (exitCode == 0) {
      println("All tests successful.")
    }

    val tests = results.map(_.results.length).sum
    println("Files=" + testNames.length + ", Tests=" + tests)

    val todoSucceeded = resultMap.mapValues { r =>
      r.results.filter { t =>
        t.directive match {
          case Some(tap.TodoDirective(_)) => t.passed
          case _                      => false
        }
      }
    }.filter(_._2.length > 0)

    val testsFailed = resultMap.mapValues { r =>
      r.results.filter { t =>
        t.directive match {
          case None => !t.passed
          case _    => false
        }
      }
    }.filter(_._2.length > 0)

    if (todoSucceeded.nonEmpty || testsFailed.nonEmpty) {
      println("")
      println("Test Summary Report")
      println("-------------------")
      for (name <- (todoSucceeded ++ testsFailed).keys) {
        val result = resultMap(name)
        println(
          name + (" " * (maxLength - name.length)) + "               " +
          "(Tests: " + result.results.length + " " +
          "Failed: " + testsFailed.getOrElse(name, Seq()).length + ")"
        )
        if (testsFailed.isDefinedAt(name)) {
          val fails = testsFailed(name)
          println(
            "  Failed test" + (if (fails.length > 1) "s" else "") + ": " +
            fails.map(_.number).mkString(", ")
          )
        }
        if (todoSucceeded.isDefinedAt(name)) {
          println(
            "  TODO passed: " +
            todoSucceeded(name).map(_.number).mkString(", ")
          )
        }
        if (exitMap(name) != 0) {
          println("  Non-zero exit status: " + exitMap(name))
        }
      }
    }

    if (exitCode == 0) {
      println("Result: PASS")
    }
    else {
      println("Result: FAIL")
      val failedFiles = results.filter { r =>
        r.results.exists { t =>
          t.directive match {
            case None => !t.passed
            case _    => false
          }
        }
      }.length
      val failedTests = testsFailed.mapValues(_.length).values.sum
      println(
        "Failed " + failedFiles + "/" + testNames.length + " test programs. " +
        failedTests + "/" + tests + " subtests failed."
      )
    }

    exitCode
  }
}
