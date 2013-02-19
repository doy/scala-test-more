package testbuilder

import java.io.OutputStream

class Builder (plan: Option[Int], out: OutputStream) {
  if (plan.isDefined) {
    Console.withOut(out) {
      println(tap.plan(plan.get))
    }
  }

  def this (plan: Int, out: OutputStream = System.out) =
    this(Some(plan), out)

  def this (out: OutputStream = System.out) =
    this(None, out)

  def ok (test: Boolean, description: String) {
    ok(test, Some(description))
  }

  def ok (test: Boolean, description: Option[String] = None) {
    val line = tap.result(test, state.currentTest, description)
    state.ok(test)
    Console.withOut(out) {
      println(line)
    }
  }

  def diag (message: String) {
    val line = tap.comment(message)
    Console.withOut(out) {
      println(line)
    }
  }

  def doneTesting () {
    if (plan.isEmpty) {
      Console.withOut(out) {
        println(tap.plan(state.currentTest - 1))
      }
    }

    if (!isPassing) {
      if (state.currentTest == 1) {
        diag("No tests run!")
      }
      else {
        val count = state.failCount
        val fails = count + " test" + (if (count > 1) "s" else "")
        val total = state.currentTest - 1
        diag("Looks like you failed " + fails + " of " + total + ".")
      }
    }
  }

  def isPassing: Boolean =
    state.isPassing

  private val state = new TestState

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
      failCount + passCount + 1

    def isPassing: Boolean =
      currentTest > 1 && failCount == 0
  }
}
