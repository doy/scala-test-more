package org.perl8.test

import scala.util.matching.Regex

import org.perl8.test.tap.TestBuilder

class TestMore (plan: Option[Plan] = None) extends Test with DelayedInit {
  def this (plan: Plan) =
    this(Some(plan))

  def delayedInit (body: => Unit) {
    testBody = { terminalInUse =>
      todo    = NoMessage
      builder = new TestBuilder(plan, terminalInUse)
      body
    }
  }

  def runTests (terminalInUse: Boolean): Int = {
    if (testBody == null) {
      delayedInit { }
    }

    testBody(terminalInUse)
    builder.doneTesting
    builder.exitCode
  }

  def ok (cond: Boolean, desc: Message = NoMessage): Boolean = {
    builderOk(cond, desc)
    if (!cond) {
      failed(desc)
    }
    cond
  }

  def is[T] (got: T, expected: T, desc: Message = NoMessage): Boolean = {
    val cond = got == expected
    builderOk(cond, desc)
    if (!cond) {
      val reason =
        "         got: '" + got + "'\n" +
        "    expected: '" + expected + "'\n"
      failed(desc, reason)
    }
    cond
  }

  def isnt[T] (got: T, expected: T, desc: Message = NoMessage): Boolean = {
    val cond = got != expected
    builderOk(cond, desc)
    if (!cond) {
      val reason =
        "         got: '" + got + "'\n" +
        "    expected: anything else\n"
      failed(desc, reason)
    }
    cond
  }

  def like (got: String, rx: Regex, desc: Message = NoMessage): Boolean = {
    val cond = rx.findFirstIn(got).nonEmpty
    builderOk(cond, desc)
    if (!cond) {
      val reason =
        "                  '" + got + "'\n" +
        "    doesn't match '" + rx + "'\n"
      failed(desc, reason)
    }
    cond
  }

  def unlike (got: String, rx: Regex, desc: Message = NoMessage): Boolean = {
    val cond = rx.findFirstIn(got).isEmpty
    builderOk(cond, desc)
    if (!cond) {
      val reason =
        "                  '" + got + "'\n" +
        "          matches '" + rx + "'\n"
      failed(desc, reason)
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
      builder = oldBuilder.cloneForSubtest(plan)
      body
      builder.doneTesting
    }
    finally {
      builder = oldBuilder
    }
    ok(success, name)
  }

  protected def ignoreFrame (frame: StackTraceElement): Boolean = {
    val className = frame.getClassName
    val methodName = frame.getMethodName

    // ignore everything in this class, except the hideTestMethod call which we
    // use as a stack trace marker
    (className == "org.perl8.test.TestMore" &&
      methodName != "hideTestMethod") ||
    // when you call a method in a class when the method is defined in a
    // trait, it calls a stub which calls the real definition in the trait.
    // the trait is represented under the hood as a class with the same name
    // as the trait, except with $class appended. this is a gross reliance on
    // implementation details that could change at any moment, but i don't
    // really see any better options.
    """\$class$""".r.findFirstIn(className).nonEmpty
  }

  private def builderOk (cond: Boolean, desc: Message) {
    builder.ok(cond, desc.map(d => "- " + d), todo)
  }

  private def failed (desc: Message, reason: String) {
    failed(desc, Some(reason))
  }

  private def failed (desc: Message, reason: Option[String] = None) {
    val stack = Thread.currentThread.getStackTrace.drop(1).filter { frame =>
      !ignoreFrame(frame)
    }
    val idx = stack.lastIndexWhere { frame =>
      frame.getClassName == "org.perl8.test.TestMore" &&
      frame.getMethodName == "hideTestMethod"
    }
    val caller = idx match {
      case -1 => stack.headOption
      // one level to jump out of hideTestMethod and one level to jump out of
      // the method that called hideTestMethod
      case i  => stack.drop(i + 2).headOption

    }
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
    builder.diag(message + trace + reason.map("\n" + _).getOrElse(""))
  }

  // this just adds a method call with a known name to the stack trace, so
  // that we can detect it later
  def hideTestMethod[T] (body: => T): T = {
    body
  }

  private var todo: Message             = _
  private var builder: TestBuilder      = _
  private var testBody: Boolean => Unit = _
}
