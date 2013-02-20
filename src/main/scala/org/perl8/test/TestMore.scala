package org.perl8.test

import org.perl8.test.Utils._

import java.io.OutputStream
import scala.util.matching.Regex

class TestMore (
  plan: Option[Plan],
  out:  OutputStream
) extends Test with DelayedInit {
  def this (plan: Plan, out: OutputStream = System.out) =
    this(Some(plan), out)

  def this (out: OutputStream = System.out) =
    this(None, out)

  def delayedInit (body: => Unit) {
    todo = NoMessage
    builder = new TestBuilder(plan, out, 0, NoMessage)
    testBody = () => body
  }

  def run (): Int = {
    testBody()
    builder.doneTesting
    if (builder.isPassing) 0 else 1
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

  private def failed (desc: Message) {
    val caller = Thread.currentThread.getStackTrace.drop(1).find(frame => {
      frame.getFileName != "TestMore.scala"
    })
    val (file, line) = caller match {
      case Some(frame) => (frame.getFileName, frame.getLineNumber)
      case None        => ("<unknown file>", "<unknown line>")
    }
    val message = "  Failed test" + (desc match {
      case HasMessage(m) => " '" + m + "'\n  "
      case NoMessage     => " "
    })
    val trace = "at " + file + " line " + line + "."
    builder.diag(message + trace)
  }

  private var todo: Message        = _
  private var builder: TestBuilder = _
  private var testBody: () => Unit = _
}
