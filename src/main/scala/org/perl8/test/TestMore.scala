package org.perl8.test

import scala.util.matching.Regex

import org.perl8.test.tap.TestBuilder

class TestMore (plan: Plan = NoPlan) extends Test with DelayedInit {
  def delayedInit (body: => Unit) {
    testBody = { terminalInUse =>
      todo    = None
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

  def ok (cond: Boolean): Boolean =
    test(cond)

  def ok (cond: Boolean, desc: String): Boolean =
    testWithDesc(cond, desc)

  def is[T] (got: T, expected: T): Boolean =
    test(got == expected, isMessage(got, expected))

  def is[T] (got: T, expected: T, desc: String): Boolean =
    testWithDesc(got == expected, desc, isMessage(got, expected))

  def isnt[T] (got: T, expected: T): Boolean =
    test(got != expected, isntMessage(got))

  def isnt[T] (got: T, expected: T, desc: String): Boolean =
    testWithDesc(got != expected, desc, isntMessage(got))

  def like (got: String, rx: Regex): Boolean =
    test(rx.findFirstIn(got).nonEmpty, likeMessage(got, rx))

  def like (got: String, rx: Regex, desc: String): Boolean =
    testWithDesc(rx.findFirstIn(got).nonEmpty, desc, likeMessage(got, rx))

  def unlike (got: String, rx: Regex): Boolean =
    test(rx.findFirstIn(got).isEmpty, unlikeMessage(got, rx))

  def unlike (got: String, rx: Regex, desc: String): Boolean =
    testWithDesc(rx.findFirstIn(got).isEmpty, desc, unlikeMessage(got, rx))

  def pass: Boolean =
    ok(true)

  def pass (desc: String): Boolean =
    ok(true, desc)

  def fail: Boolean =
    ok(false)

  def fail (desc: String): Boolean =
    ok(false, desc)

  def diag (message: String) {
    builder.diag(message)
  }

  def BAIL_OUT {
    builder.bailOut
  }

  def BAIL_OUT (desc: String) {
    builder.bailOut(desc)
  }

  def todo (reason: String)(body: => Unit) {
    val oldTodo = todo
    try {
      todo = Some(reason)
      body
    }
    finally {
      todo = oldTodo
    }
  }

  def skip (count: Int)(body: => Unit) {
    for (i <- 1 to count) {
      builder.skip
    }
  }

  def skip (count: Int, reason: String)(body: => Unit) {
    for (i <- 1 to count) {
      builder.skip(reason)
    }
  }

  def subtest (
    name: String,
    plan: Plan = NoPlan
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

  private def isMessage[T] (got: T, expected: T): String =
    "         got: '" + got + "'\n" +
    "    expected: '" + expected + "'\n"

  private def isntMessage[T] (got: T): String =
    "         got: '" + got + "'\n" +
    "    expected: anything else\n"

  private def likeMessage (got: String, rx: Regex): String =
    "                  '" + got + "'\n" +
    "    doesn't match '" + rx + "'\n"

  private def unlikeMessage (got: String, rx: Regex): String =
    "                  '" + got + "'\n" +
    "          matches '" + rx + "'\n"

  private def testWithDesc (
    cond:   Boolean,
    desc:   String
  ): Boolean = {
    todo match {
      case Some(t) => builder.okTodo(cond, "- " + desc, t)
      case None    => builder.ok(cond, "- " + desc)
    }
    if (!cond) {
      failed(Some(desc), None)
    }
    cond
  }

  private def testWithDesc (
    cond:   Boolean,
    desc:   String,
    reason: => String
  ): Boolean = {
    todo match {
      case Some(t) => builder.okTodo(cond, "- " + desc, t)
      case None    => builder.ok(cond, "- " + desc)
    }
    if (!cond) {
      failed(Some(desc), Some(reason))
    }
    cond
  }

  private def test (cond: Boolean): Boolean = {
    todo match {
      case Some(t) => builder.okTodo(cond, t)
      case None    => builder.ok(cond)
    }
    if (!cond) {
      failed(None, None)
    }
    cond
  }

  private def test (cond: Boolean, reason: => String): Boolean = {
    todo match {
      case Some(t) => builder.okTodo(cond, t)
      case None    => builder.ok(cond)
    }
    if (!cond) {
      failed(None, Some(reason))
    }
    cond
  }

  private def failed (desc: Option[String], reason: Option[String]) {
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
      case Some(_) => "Failed (TODO) test"
      case None    => "Failed test"
    }) + (desc match {
      case Some(m) => " '" + m + "'\n  "
      case None    => " "
    })
    val trace = "at " + file + " line " + line + "."
    val explanation = message + trace + reason.map("\n" + _).getOrElse("")
    if (todo.isDefined) {
      builder.note(explanation)
    }
    else {
      builder.diag(explanation)
    }
  }

  // this just adds a method call with a known name to the stack trace, so
  // that we can detect it later
  def hideTestMethod[T] (body: => T): T = {
    body
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

  private var todo:     Option[String]  = _
  private var builder:  TestBuilder     = _
  private var testBody: Boolean => Unit = _
}
