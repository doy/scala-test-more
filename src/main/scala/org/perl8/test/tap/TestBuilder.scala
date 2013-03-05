package com.iinteractive.test.tap

import com.iinteractive.test._

/** This class provides a convenient yet low level API for generating TAP
  * streams. Each instance of this class handles a single TAP stream, and
  * keeps track of things like the current test number for you. All
  * TAP-producing methods write the TAP lines to `Console.out` or
  * `Console.err`, so you can override those (via `Console.withOut` or
  * `Console.withErr`).
  */
class TestBuilder private (
  plan:          Plan,
  indent:        String,
  terminalInUse: Boolean
) {
  plan match {
    case NoPlan => ()
    case p      => outLine(Producer.plan(p))
  }

  /** Creates a new builder instance, and emits the corresponding plan line,
    * unless the plan is not given.
    *
    * @param plan [[com.iinteractive.test.Plan plan]] for this test.
    * @param terminalInUse Whether this test is being run from a harness which
    *                      will not just be writing directly to the output.
    *                      This will make things written to `Console.err` have
    *                      a newline prepended, so that they always start on
    *                      an empty line.
    */
  def this (plan: Plan = NoPlan, terminalInUse: Boolean = false) =
    this(plan, "", terminalInUse)

  /** Create a new TestBuilder instance, to be used to run a subtest. This new
    * instance will have all of its lines prefixed by an additional level of
    * indentation. This instance will still need to have `doneTesting`
    * called on it, and the result of the subtest will still need to be
    * reported as a separate test result through `ok`.
    */
  def cloneForSubtest (newPlan: Plan): TestBuilder =
    new TestBuilder(newPlan, indent + "    ", terminalInUse)

  /** Reports a single test result to `Console.out`. */
  def ok (test: Boolean) {
    state.ok(test)
    outLine(Producer.result(test, state.currentTest))
  }

  /** Reports a single test result with description to `Console.out`. */
  def ok (test: Boolean, description: String) {
    state.ok(test)
    outLine(Producer.result(test, state.currentTest, description))
  }

  /** Reports a single TODO test result to `Console.out`. */
  def todo (todo: String, test: Boolean) {
    state.ok(true)
    outLine(Producer.todoResult(test, state.currentTest, todo))
  }

  /** Reports a single TODO test result with description to `Console.out`. */
  def todo (todo: String, test: Boolean, description: String) {
    state.ok(true)
    outLine(Producer.todoResult(test, state.currentTest, description, todo))
  }

  /** Reports a single skipped test result to `Console.out`. */
  def skip (reason: String) {
    state.ok(true)
    outLine(Producer.skip(state.currentTest, reason))
  }

  /** Writes a comment line to `Console.err`. This will allow it to be
    * visible in most summarizing harnesses (which consume and parse
    * everything that goes to `Console.out`).
    */
  def diag (message: String) {
    errLine(Producer.comment(message))
  }

  /** Write a comment line to `Console.out`. This will typically only be
    * visible in the raw TAP stream.
    */
  def note (message: String) {
    outLine(Producer.comment(message))
  }

  /** Abort the current test, with a message. */
  def bailOut (message: String) {
    val bailOutMessage = Producer.bailOut(message)
    outLine(bailOutMessage)
    throw new BailOutException(bailOutMessage)
  }

  /** Finalize the current builder instance. This writes the auto-calculated
    * plan to `Console.out` if the plan type was `NoPlan` and reports a
    * summary of the test results as a comment to `Console.err`.
    *
    * @return whether or not the test class as a whole passed.
    */
  def doneTesting: Boolean = {
    plan match {
      case NumericPlan(_) => printErrors
      case SkipAll(_)     => ()
      case NoPlan         => {
        outLine(Producer.plan(state.currentTest))
        printErrors
      }
    }
    state.isPassing
  }

  /** The exit code to use, in harnesses that run a single test. Passing tests
    * return 0, invalid tests (such as running a different number of tests
    * than planned) return 255, and all others return the number of failed
    * tests.
    */
  def exitCode: Int =
    if (state.isPassing) {
      0
    }
    else if (!state.matchesPlan || state.currentTest == 0) {
      255
    }
    else {
      state.failCount
    }

  private def printErrors {
    if (!state.matchesPlan) {
      val planCount = (plan match {
        case NoPlan  => state.currentTest
        case p       => p.plan
      })
      val planned = planCount + " test" + (if (planCount > 1) "s" else "")
      val ran = state.currentTest
      diag("Looks like you planned " + planned + " but ran " + ran + ".")
    }

    if (state.currentTest == 0) {
      diag("No tests run!")
    }

    if (state.failCount > 0) {
      val count = state.failCount
      val fails = count + " test" + (if (count > 1) "s" else "")
      val total =
        state.currentTest + (if (state.matchesPlan) "" else " run")
      diag("Looks like you failed " + fails + " of " + total + ".")
    }
  }

  private val state = new TestState

  private def outLine (str: String) {
    Console.out.println(withIndent(str))
  }

  private def errLine (str: String) {
    if (terminalInUse) {
      Console.err.print("\n")
    }
    Console.err.println(withIndent(str))
  }

  private def withIndent (str: String): String =
    str.split("\n").map(s => indent + s).mkString("\n")

  private class TestState {
    var passCount = 0
    var failCount = 0

    def ok (cond: Boolean) {
      if (cond) {
        passCount += 1
      }
      else {
        failCount += 1
      }
    }

    def currentTest: Int =
      failCount + passCount

    def matchesPlan: Boolean = plan match {
      case NumericPlan(p) => p.plan == currentTest
      case _              => true
    }

    def isPassing: Boolean = plan match {
      case SkipAll(_) => true
      case _          => currentTest > 0 && failCount == 0 && matchesPlan
    }
  }
}
