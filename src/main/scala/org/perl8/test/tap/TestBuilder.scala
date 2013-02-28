package org.perl8.test.tap

import org.perl8.test._

class TestBuilder private (
  plan:          Plan,
  indent:        String,
  terminalInUse: Boolean
) {
  plan match {
    case NoPlan => ()
    case p      => outLine(Producer.plan(p))
  }

  def this (plan: Plan = NoPlan, terminalInUse: Boolean = false) =
    this(plan, "", terminalInUse)

  def cloneForSubtest (newPlan: Plan): TestBuilder =
    new TestBuilder(newPlan, indent + "    ", terminalInUse)

  def ok (test: Boolean) {
    state.ok(test)
    outLine(Producer.result(test, state.currentTest))
  }

  def ok (test: Boolean, description: String) {
    state.ok(test)
    outLine(Producer.result(test, state.currentTest, description))
  }

  def okTodo (test: Boolean, todo: String) {
    state.ok(true)
    outLine(Producer.todoResult(test, state.currentTest, todo))
  }

  def okTodo (test: Boolean, description: String, todo: String) {
    state.ok(true)
    outLine(Producer.todoResult(test, state.currentTest, description, todo))
  }

  def skip {
    state.ok(true)
    outLine(Producer.skip(state.currentTest))
  }

  def skip (reason: String) {
    state.ok(true)
    outLine(Producer.skip(state.currentTest, reason))
  }

  def diag (message: String) {
    errLine(Producer.comment(message))
  }

  def note (message: String) {
    outLine(Producer.comment(message))
  }

  def bailOut {
    val bailOutMessage = Producer.bailOut
    outLine(bailOutMessage)
    throw new BailOutException(bailOutMessage)
  }

  def bailOut (message: String) {
    val bailOutMessage = Producer.bailOut(message)
    outLine(bailOutMessage)
    throw new BailOutException(bailOutMessage)
  }

  def doneTesting: Boolean = {
    plan match {
      case NoPlan => outLine(Producer.plan(state.currentTest))
      case _      => ()
    }

    if (!state.isPassing) {
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

    state.isPassing
  }

  def failedTests: Int =
    state.failCount

  def exitCode: Int =
    if (state.isPassing) {
      0
    }
    else if (!state.matchesPlan) {
      255
    }
    else {
      state.failCount
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

    def isPassing: Boolean =
      currentTest > 0 && failCount == 0 && matchesPlan
  }
}
