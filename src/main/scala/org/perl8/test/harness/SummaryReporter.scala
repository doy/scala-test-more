package org.perl8.test.harness

import org.perl8.test.tap.{TAPEvent,StartEvent,ResultEvent,PlanEvent,EndEvent}
import org.perl8.test.tap.{TAPResult,TodoDirective}
import org.perl8.test.Test

/** Runs a series of tests. The TAP output from these tests is parsed, and
  * output is produced which is similar in style to Perl's
  * [[https://metacpan.org/module/Test::Harness Test::Harness]].
  */
class SummaryReporter extends MultiTestReporter with SummarizedTests {
  def run (testNames: Seq[String]): Int = {
    val results = runTests(testNames)
    val success = results.values.forall(_.success)
    printTestSummary(success, results)
    if (success) 0 else 1
  }

  protected def runTests (testNames: Seq[String]): Map[String, TAPResult] = {
    val maxLength = testNames.map(_.length).max

    testNames.map { name =>
      val callbackGenerator: () => TAPEvent => Unit = () => {
        var width             = 0
        var tests             = 0
        var plan: Option[Int] = None

        def status = {
          tests + "/" + plan.getOrElse("?")
        }

        def printStatus (st: String) {
          print("\r" + (" " * width) + "\r")
          val line =
            name + " " + ("." * (maxLength - name.length)) + ".. " + st
          width = line.length
          print(line)
          Console.out.flush
        }

        (e: TAPEvent) => e match {
          case StartEvent => {
            printStatus("")
          }
          case PlanEvent(p) => {
            plan = Some(p.plan)
            printStatus(status)
          }
          case ResultEvent(r) => {
            tests += 1
            printStatus(status)
          }
          case EndEvent(result) => {
            if (result.success) {
              printStatus("")
              println("ok")
            }
            else {
              val results = result.results.length
              val failed = result.results.count { t =>
                !t.passed && !t.directive.isDefined
              }

              printStatus("")
              println("Dubious, test returned " + result.exitCode)
              println("Failed " + failed + "/" + results + " subtests")
            }
          }
          case _ => ()
        }
      }

      name -> runOneTest(newInstance[Test](name), callbackGenerator())
    }.toMap
  }

  protected def printTestSummary (
    success: Boolean,
    results: Map[String, TAPResult]
  ) {
    printSuccess(success)
    printLongSummary(results)
    printShortSummary(results)
    printPassFail(success, results)
  }

  private def printSuccess (success: Boolean) {
    if (success) {
      println("All tests successful.")
    }
  }

  private def printShortSummary (results: Map[String, TAPResult]) {
    val files = results.size
    val tests = results.values.map(_.results.length).sum
    println("Files=" + files + ", Tests=" + tests)
  }

  private def printLongSummary (results: Map[String, TAPResult]) {
    val todoSucceeded = results.mapValues { r =>
      r.results.filter { t =>
        t.directive match {
          case Some(TodoDirective(_)) => t.passed
          case _                      => false
        }
      }
    }.filter(_._2.length > 0)

    val testsFailed = results.mapValues { r =>
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
        val result = results(name)

        println(
          name + (" " * (maxLength - name.length)) + "               " +
          "(Tests: " + result.results.length + " " +
          "Failed: " + testsFailed.getOrElse(name, Nil).length + ")"
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

        val exitCode = results(name).exitCode
        if (exitCode != 0) {
          println("  Non-zero exit status: " + exitCode)
        }
      }
    }
  }

  private def printPassFail (
    success: Boolean,
    results: Map[String, TAPResult]
  ) {
    if (success) {
      println("Result: PASS")
    }
    else {
      println("Result: FAIL")

      val testResults = results.values

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
