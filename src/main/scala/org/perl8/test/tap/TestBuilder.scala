package org.perl8.test.tap

import org.perl8.test._

class TestBuilder (
  plan:              Option[Plan],
  val indent:        String,
  val terminalInUse: Boolean
) {
  plan.foreach(p => outLine(Producer.plan(p)))

  def this (plan: Plan, indent: String = "", terminalInUse: Boolean = false) =
    this(Some(plan), indent, terminalInUse)

  def this (indent: String = "", terminalInUse: Boolean = false) =
    this(None, indent, terminalInUse)

  def ok (
    test:        Boolean,
    description: Message = NoMessage,
    todo:        Message = NoMessage
  ) {
    val line = Producer.result(test, state.currentTest, description, todo)
    state.ok(test || todo.isDefined)
    outLine(line)
  }

  def skip (reason: Message = NoMessage) {
    val line = Producer.skip(state.currentTest, reason)
    state.ok(true)
    outLine(line)
  }

  def diag (message: Message) {
    message.foreach { m =>
      errLine(Producer.comment(m))
    }
  }

  def note (message: Message) {
    message.foreach(m => outLine(Producer.comment(m)))
  }

  def bailOut (message: Message = NoMessage) {
    outLine(Producer.bailOut(message))
    throw new BailOutException(message.getOrElse(""))
  }

  def doneTesting: Boolean = {
    plan match {
      case None => outLine(Producer.plan(state.currentTest - 1))
      case _    => ()
    }

    if (!state.isPassing) {
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
      failCount + passCount + 1

    def matchesPlan: Boolean = plan.forall { p =>
      p.plan == failCount + passCount
    }

    def isPassing: Boolean =
      currentTest > 1 && failCount == 0 && matchesPlan
  }
}
