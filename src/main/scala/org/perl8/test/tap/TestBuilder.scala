package org.perl8.test.tap

import java.io.OutputStream

import org.perl8.test.Utils._

class TestBuilder (
  plan:             Option[Plan],
  out:              OutputStream,
  val indent:       Int,
  private val name: Message
) {
  plan.foreach(p => println(Producer.plan(p)))

  def this (
    plan:   Plan,
    out:    OutputStream = System.out,
    indent: Int          = 0,
    name:   Message      = NoMessage
  ) =
    this(Some(plan), out, indent, name)

  def this (
    out:    OutputStream = System.out,
    indent: Int          = 0,
    name:   Message      = NoMessage
  ) =
    this(None, out, indent, name)

  def ok (
    test:        Boolean,
    description: Message = NoMessage,
    todo:        Message = NoMessage
  ) {
    val line = Producer.result(test, state.currentTest, description, todo)
    state.ok(test || todo.isDefined)
    println(line)
  }

  def skip (reason: Message = NoMessage) {
    val line = Producer.skip(state.currentTest, reason)
    state.ok(true)
    println(line)
  }

  def diag (message: Message) {
    message.foreach(m => println(Producer.comment(m)))
  }

  def bailOut (message: Message = NoMessage) {
    println(Producer.bailOut(message))
    throw new BailOutException(message.getOrElse(""))
  }

  def doneTesting () {
    plan match {
      case None => println(Producer.plan(state.currentTest - 1))
      case _    => ()
    }

    if (!isPassing) {
      if (!state.matchesPlan) {
        val planCount = (plan match {
          case Some(p) => p.plan
          case None    => state.currentTest - 1
        })
        val planned = planCount + " test" + (if (planCount > 1) "s" else "")
        val ran = state.currentTest - 1
        diag("Looks like you planned " + planned + " but ran " + ran + ".")
      }

      if (state.currentTest == 1) {
        diag("No tests run!")
      }

      if (state.failCount > 0) {
        val count = state.failCount
        val fails = count + " test" + (if (count > 1) "s" else "")
        val total =
          state.currentTest - 1 + (if (state.matchesPlan) "" else " run")
        diag("Looks like you failed " + fails + " of " + total + ".")
      }
    }
  }

  def isPassing: Boolean =
    state.isPassing

  def failedTests: Int =
    state.failCount

  private val state = new TestState

  private def println (str: String) {
    Console.withOut(out) {
      val indented =
        str.split("\n").map(s => (" " * (indent * 4)) + s).mkString("\n")
      Console.println(indented)
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

    def matchesPlan: Boolean = plan match {
      case Some(p) => p.plan == failCount + passCount
      case None    => true
    }

    def isPassing: Boolean =
      currentTest > 1 && failCount == 0 && matchesPlan
  }
}
