package org.perl8.test

import scala.util.matching.Regex

import org.perl8.test.tap.TestBuilder

class TestMore (plan: Option[Plan] = None) extends Test with DelayedInit {
  def this (plan: Plan) =
    this(Some(plan))

  def delayedInit (body: => Unit) {
    level    = 0
    todo     = NoMessage
    builder  = new TestBuilder(plan, "", NoMessage)
    testBody = () => body
  }

  def run () {
    if (testBody == null) {
      delayedInit { }
    }

    testBody()
    builder.doneTesting
  }

  def ok (cond: Boolean, desc: Message = NoMessage): Boolean = {
    builder.ok(cond, desc.map(d => "- " + d), todo)
    if (!cond) {
      failed(desc)
    }
    cond
  }

  def is[T] (got: T, expected: T, desc: Message = NoMessage): Boolean = {
    val cond = ok(got == expected, desc)
    if (!cond) {
      builder.diag("         got: '" + got + "'")
      builder.diag("    expected: '" + expected + "'")
    }
    cond
  }

  def isnt[T] (got: T, expected: T, desc: Message = NoMessage): Boolean = {
    val cond = ok(got != expected, desc)
    if (!cond) {
      builder.diag("         got: '" + got + "'")
      builder.diag("    expected: anything else")
    }
    cond
  }

  def like (got: String, rx: Regex, desc: Message = NoMessage): Boolean = {
    val cond = ok(rx.findFirstIn(got).nonEmpty, desc)
    if (!cond) {
      builder.diag("                  '" + got + "'")
      builder.diag("    doesn't match '" + rx + "'")
    }
    cond
  }

  def unlike (got: String, rx: Regex, desc: Message = NoMessage): Boolean = {
    val cond = ok(rx.findFirstIn(got).isEmpty, desc)
    if (!cond) {
      builder.diag("                  '" + got + "'")
      builder.diag("          matches '" + rx + "'")
    }
    cond
  }

  def pass (desc: Message = NoMessage): Boolean =
    ok(true, desc)

  def fail (desc: Message = NoMessage): Boolean =
    ok(false, desc)

  def diag (message: String) {
    builder.diag(message)
  }

  def BAIL_OUT (desc: Message = NoMessage) {
    builder.bailOut(desc)
  }

  def todo (reason: Message = NoMessage)(body: => Unit) {
    val oldTodo = todo
    try {
      todo = reason
      body
    }
    finally {
      todo = oldTodo
    }
  }

  def skip (count: Int, reason: Message = NoMessage)(body: => Unit) {
    for (i <- 1 to count) {
      builder.skip(reason)
    }
  }

  def subtest (name: Message, plan: Plan)(body: => Unit): Boolean =
    subtest(name, Some(plan))(body)

  def subtest (
    name: Message,
    plan: Option[Plan] = None
  )(body: => Unit): Boolean = {
    val oldBuilder = builder
    val success = try {
      builder = new TestBuilder(
        plan,
        oldBuilder.indent + "    ",
        name.map(n => "- " + n)
      )
      body
      builder.doneTesting
    }
    finally {
      builder = oldBuilder
    }
    ok(success, name)
  }

  private def failed (desc: Message) {
    val stack = Thread.currentThread.getStackTrace.drop(1)
    def findIdx (level: Int, start: Int): Int = {
      val idx = stack.indexWhere({ frame =>
        frame.getFileName != "TestMore.scala"
      }, start)

      if (level == 0) { idx } else { findIdx(level - 1, idx + 1) }
    }
    val idx = findIdx(level, 0)
    val caller = stack.drop(idx).headOption
    val (file, line) = caller match {
      case Some(frame) => (frame.getFileName, frame.getLineNumber)
      case None        => ("<unknown file>", "<unknown line>")
    }
    val message = "  " + (todo match {
      case HasMessage(_) => "Failed (TODO) test"
      case NoMessage     => "Failed test"
    }) + (desc match {
      case HasMessage(m) => " '" + m + "'\n  "
      case NoMessage     => " "
    })
    val trace = "at " + file + " line " + line + "."
    builder.diag(message + trace)
  }

  def withLevel[T] (newLevel: Int)(body: => T): T = {
    val oldLevel = level
    try {
      // XXX "+4" is something of a hack, not sure how stable it will be
      level += newLevel + 4
      body
    }
    finally {
      level = oldLevel
    }
  }

  private var level: Int           = _
  private var todo: Message        = _
  private var builder: TestBuilder = _
  private var testBody: () => Unit = _
}
