package testbuilder

import java.io.OutputStream

import util._

class Builder (plan: Option[Plan], out: OutputStream) {
  Console.withOut(out) {
    plan.foreach(p => println(tap.plan(p)))
  }

  def this (plan: Plan, out: OutputStream = System.out) =
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

  def bailOut (message: String) {
    bailOut(Some(message))
  }

  def bailOut (message: Option[String] = None) {
    val line = tap.bailOut(message)
    Console.withOut(out) {
      println(line)
    }
    throw new BailOutException(message.getOrElse(""))
  }

  def doneTesting () {
    Console.withOut(out) {
      plan match {
        case None => println(tap.plan(state.currentTest - 1))
        case _    => ()
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
