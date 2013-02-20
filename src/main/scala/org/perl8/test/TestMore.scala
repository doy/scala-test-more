package org.perl8.test

import org.perl8.test.Utils._

import java.io.OutputStream

class TestMore (
  plan: Option[Plan],
  out:  OutputStream
) extends Test with DelayedInit {
  def this (plan: Plan, out: OutputStream = System.out) =
    this(Some(plan), out)

  def this (out: OutputStream = System.out) =
    this(None, out)

  def delayedInit (body: => Unit) {
    builder = new TestBuilder(plan, out, 0, NoMessage)
    testBody = () => body
  }

  def run (): Int = {
    testBody()
    builder.doneTesting
    if (builder.isPassing) 0 else 1
  }

  def ok (cond: Boolean, desc: Message = NoMessage): Boolean = {
    builder.ok(cond, desc.map(d => "- " + d))
    if (!cond) {
      val caller = Thread.currentThread.getStackTrace()(0)
      builder.diag("  Failed test '" + desc + "'")
      builder.diag(
        "  at " + caller.getFileName + " line " + caller.getLineNumber + "."
      )
    }
    cond
  }

  private var builder: TestBuilder = _
  private var testBody: () => Unit = _
}
