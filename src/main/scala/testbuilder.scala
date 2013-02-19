package testbuilder

import java.io.OutputStream

class Builder (plan: Option[Int], out: OutputStream) {
  plan.foreach(p => {
    Console.withOut(out) {
      println(tap.plan(p))
    }
  })

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
    if (noPlan) {
      Console.withOut(out) {
        println(tap.plan(state.currentTest - 1))
      }
    }
  }

  def isPassing: Boolean =
    state.isPassing

  private val state = new TestState

  private def noPlan =
    plan.isEmpty

  private class TestState {
    private var passCount = 0
    private var failCount = 0

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
