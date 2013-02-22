package org.perl8.test.harness

import java.io.{OutputStream,ByteArrayOutputStream}

import org.perl8.test.Test
import org.perl8.test.tap
import org.perl8.test.tap.{TAPResult,TodoDirective}
import Utils._

class SummaryReporter extends MultiTestReporter {
  def run (testNames: Seq[String]): Int = {
    val results = runTests(testNames)
    val success = results.values.map(_._1).forall(_ == 0)
    printTestSummary(success, results)
    if (success) 0 else 1
  }

  def runTests (testNames: Seq[String]): Map[String, (Int, TAPResult)] = {
    val maxLength = testNames.map(_.length).max

    testNames.map { name =>
      print(name + ("." * (maxLength - name.length)) + "... ")

      val out = new ByteArrayOutputStream
      val test = newInstance[Test, OutputStream](name, out)
      val exitCode = test.run
      val result = tap.Consumer.parse(out)

      if (exitCode == 0) {
        println("ok")
      }
      else {
        val results = result.results.length
        val failed = result.results.count { t =>
          !t.passed && !t.directive.isDefined
        }

        println("Dubious, test returned " + exitCode)
        println("Failed " + failed + "/" + results + " subtests")
      }

      name -> (exitCode, result)
    }.toMap
  }

  def printTestSummary (
    success: Boolean,
    results: Map[String, (Int, TAPResult)]
  ) {
    printSuccess(success)
    printShortSummary(results)
    printLongSummary(results)
    printPassFail(success, results)
  }

  private def printSuccess (success: Boolean) {
    if (success) {
      println("All tests successful.")
    }
  }

  private def printShortSummary (results: Map[String, (Int, TAPResult)]) {
    val files = results.size
    val tests = results.values.map(_._2.results.length).sum
    println("Files=" + files + ", Tests=" + tests)
  }

  private def printLongSummary (results: Map[String, (Int, TAPResult)]) {
    val resultMap = results.map { case (s, r) => s -> r._2 }

    val todoSucceeded = resultMap.mapValues { r =>
      r.results.filter { t =>
        t.directive match {
          case Some(TodoDirective(_)) => t.passed
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

    val testNames = (todoSucceeded ++ testsFailed).keys

    if (testNames.nonEmpty) {
      println("")
      println("Test Summary Report")
      println("-------------------")

      val maxLength = testNames.map(_.length).max

      for (name <- testNames) {
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
          val todos = todoSucceeded(name)
          println(
            "  TODO passed: " +
            todos.map(_.number).mkString(", ")
          )
        }

        val exitCode = results(name)._1
        if (exitCode != 0) {
          println("  Non-zero exit status: " + exitCode)
        }
      }
    }
  }

  private def printPassFail (
    success: Boolean,
    results: Map[String, (Int, TAPResult)]
  ) {
    if (success) {
      println("Result: PASS")
    }
    else {
      println("Result: FAIL")

      val testResults = results.values.map(_._2)

      val testsFailed = testResults.map { r =>
        r.results.count { t =>
          t.directive match {
            case None => !t.passed
            case _    => false
          }
        }
      }.filter(_ > 0)
      val failedFiles = testsFailed.size
      val failedTests = testsFailed.sum

      val allFiles = testResults.size
      val allTests = testResults.map(_.results.length).sum

      println(
        "Failed " + failedFiles + "/" + allFiles + " test programs. " +
        failedTests + "/" + allTests + " subtests failed."
      )
    }
  }
}
