package testbuilder

import java.io.OutputStream

import util._

class Builder (plan: Option[Plan], out: OutputStream) {
  plan.foreach(p => println(tap.plan(p)))

  def this (plan: Plan, out: OutputStream = System.out) =
    this(Some(plan), out)

  def this (out: OutputStream = System.out) =
    this(None, out)

  def ok (test: Boolean, description: String, todo: String) {
    ok(test, Some(description), Some(todo))
  }

  def ok (test: Boolean, description: String) {
    ok(test, Some(description))
  }

  def ok (
    test:        Boolean,
    description: Option[String] = None,
    todo:        Option[String] = None
  ) {
    val line = tap.result(test, state.currentTest, description, todo)
    state.ok(test || todo.isDefined)
    println(line)
  }

  def skip (reason: String) {
    skip(Some(reason))
  }

  def skip (reason: Option[String] = None) {
    val line = tap.skip(state.currentTest, reason)
    state.ok(true)
    println(line)
  }

  def diag (message: String) {
    println(tap.comment(message))
  }

  def bailOut (message: String) {
    bailOut(Some(message))
  }

  def bailOut (message: Option[String] = None) {
    println(tap.bailOut(message))
    throw new BailOutException(message.getOrElse(""))
  }

  def doneTesting () {
    plan match {
      case None => println(tap.plan(state.currentTest - 1))
      case _    => ()
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

  private def println (str: Any) {
    Console.withOut(out) {
      Console.println(str)
    }
  }

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
