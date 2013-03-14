package com.iinteractive.test

import scala.util.matching.Regex

import com.iinteractive.test.tap.TestBuilder

/** This class is an implementation of the excellent
  * [[https://metacpan.org/module/Test::More Test::More]] testing library for
  * Perl. It provides a simple assertion-based testing API, which produces
  * [[http://en.wikipedia.org/wiki/Test_Anything_Protocol TAP]], which can be
  * parsed by any TAP consumer. This library includes several TAP-consuming
  * harnesses to use with tests using this class, including one that supports
  * testing via `sbt test`.
  *
  * ==Basics==
  *
  * The most basic test looks like this:
  *
  * <pre>
  * class MyTest extends TestMore {
  *   ok(true)
  * }
  * </pre>
  *
  * This runs a test containing a single assertion. This will generate a TAP
  * stream that looks like this:
  *
  * <pre>
  * ok 1
  * 1..1
  * </pre>
  *
  * which can be parsed by one of the test harnesses provided by this library.
  *
  * ==Running tests==
  *
  * The simplest way to run tests is through sbt. You can register this
  * framework with sbt by adding this line to your `build.sbt` file:
  *
  * <pre>
  * testFrameworks += new TestFramework("com.iinteractive.test.sbt.Framework")
  * </pre>
  *
  * Then, any classes in your test directory which extend `TestMore` will be
  * automatically detected and run.
  *
  * ==Assertions==
  *
  * This class contains many more assertion methods than just `ok`. Here is a
  * more extensive example (borrowed from Test::More's documentation):
  *
  * <pre>
  * class MyTest extends TestMore {
  *   ok(got == expected, testName)
  *
  *   is(got, expected, testName)
  *   isnt(got, expected, testName)
  *
  *   diag("here's what went wrong")
  *
  *   like(got, """expected""".r, testName)
  *   unlike(got, """expected""".r, testName)
  *
  *   skip(howMany, why) {
  *     ok(foo(), testName)
  *     is(foo(42), 23, testName)
  *   }
  *
  *   todo(why) {
  *     ok(foo(), testName)
  *     is(foo(42), 23, testName)
  *   }
  *
  *   pass(testName)
  *   fail(testName)
  *
  *   BAIL_OUT(why)
  * }
  * </pre>
  *
  * The difference between the simple `ok` method and the more specific
  * methods like `is` and `like` is in how failures are reported. If you write
  * this:
  *
  * <pre>
  * ok(1 == 2)
  * </pre>
  *
  * the output will look like this:
  *
  * <pre>
  * not ok 1
  * #   Failed test at MyTest.scala line 4.
  * </pre>
  *
  * On the other hand, a more specific assertion such as:
  *
  * <pre>
  * is(1, 2)
  * </pre>
  *
  * will produce more useful output:
  *
  * <pre>
  * not ok 1
  * #   Failed test at MyTest.scala line 4.
  * #          got: 1
  * #     expected: 2
  * </pre>
  *
  * In addition to assertions, there are also several methods which take a
  * block of code to run, to modify the assertions contained in that block.
  *
  * The `todo` method runs tests which are expected to fail. If they do fail,
  * the failure is reported to the test harness as a normal succeeding test,
  * and nothing happens. If they succeed, they are still reported as a
  * succeeding test, but a message is displayed to the user indicating that
  * the todo indication can be removed.
  *
  * The `skip` method takes a block which should not be run at all. This is
  * similar to `todo`, except that it is useful for tests which could cause
  * problems if they were to actually run. Since the tests are never run, it's
  * not possible to count how many tests there should be, so this must be
  * specified as a parameter.
  *
  * The `subtest` method runs a block of assertions as though they were an
  * entirely separate test, and then reports the result of that test as a
  * single assertion in the test that called `subtest`.
  *
  * ==Test plans==
  *
  * Normally, you can run any number of assertions within your class body, and
  * the framework will assume that if no exceptions were thrown, all of the
  * assertions that were meant to be run were actually run. Sometimes,
  * however, that may not be a safe assumption, especially with heavily
  * callback-driven code. In this case, you can specify exactly how many tests
  * you intend to run, and the number of tests actually run will be checked
  * against this at the end. To declare this, give a number to the `TestMore`
  * constructor:
  *
  * <pre>
  * class MyTest extends TestMore(5) {
  *   ???
  * }
  * </pre>
  *
  * In addition, if the entire test should be skipped, you can give a plan of
  * `SkipAll()`:
  *
  * <pre>
  * class MyTest extends TestMore(SkipAll()) {
  *   ???
  * }
  * </pre>
  *
  * ==Extensions==
  *
  * These assertion methods are written with the intention of being
  * composable. You can write your own test methods which call `is` or `ok` on
  * more specific bits of data. The one issue here is that, as shown above,
  * test failure messages refer to the file and line where the `is` or `ok`
  * call was made. If you want this to instead point at the line where your
  * assertion helper method was called, you can use the `hideTestMethod`
  * method like this:
  *
  * <pre>
  * trait MyTestHelpers { this: TestMore =>
  *   def notok (cond: Boolean) = hideTestMethod {
  *     ok(!cond)
  *   }
  * }
  * </pre>
  *
  * This way, the test failure will be reported from the line where `notok`
  * was called, not from the call to `ok` in the `notok` method.
  */
class TestMore (plan: Plan = NoPlan) extends Test with DelayedInit {
  def delayedInit (body: => Unit) {
    testBody = { terminalInUse =>
      todo    = None
      builder = new TestBuilder(plan, terminalInUse)
      plan match {
        case SkipAll(_) => ()
        case _          => body
      }
    }
  }

  protected def runTests (terminalInUse: Boolean): Int = {
    if (testBody == null) {
      delayedInit { }
    }

    testBody(terminalInUse)
    builder.doneTesting
    builder.exitCode
  }

  /** Assert that a condition is true.
    *
    * @example `ok(response.isSuccess)`
    */
  def ok (cond: Boolean): Boolean =
    test(cond)

  /** Assert that a condition is true, and describe the assertion.
    *
    * @example `ok(response.isSuccess, "the response succeeded")`
    */
  def ok (cond: Boolean, desc: String): Boolean =
    testWithDesc(cond, desc)

  /** Assert that two objects are equal (using `==`).
    *
    * @example `is(response.status, 200)`
    */
  def is[T] (got: T, expected: T): Boolean =
    test(got == expected, isMessage(got, expected))

  /** Assert that two objects are equal (using `==`), and describe the
    * assertion.
    *
    * @example `is(response.status, 200, "we got a 200 OK response")`
    */
  def is[T] (got: T, expected: T, desc: String): Boolean =
    testWithDesc(got == expected, desc, isMessage(got, expected))

  /** Assert that two objects are not equal (using `!=`).
    *
    * @example `isnt(response.body, "")`
    */
  def isnt[T] (got: T, expected: T): Boolean =
    test(got != expected, isntMessage(got))

  /** Assert that two objects are not equal (using `!=`), and describe the
    * assertion.
    *
    * @example `isnt(response.body, "", "we got a response body")`
    */
  def isnt[T] (got: T, expected: T, desc: String): Boolean =
    testWithDesc(got != expected, desc, isntMessage(got))

  /** Assert that a string matches a regular expression.
    *
    * @example `like(response.header("Content-Type"), """text/x?html""".r)`
    */
  def like (got: String, rx: Regex): Boolean =
    test(rx.findFirstIn(got).nonEmpty, likeMessage(got, rx))

  /** Assert that a string matches a regular expression, and describe the
    * assertion.
    *
    * @example `like(response.header("Content-Type"), """text/x?html""".r, "we got an html content type")`
    */
  def like (got: String, rx: Regex, desc: String): Boolean =
    testWithDesc(rx.findFirstIn(got).nonEmpty, desc, likeMessage(got, rx))

  /** Assert that a string doesn't match a regular expression.
    *
    * @example `unlike(response.header("Authorization"), """^Digest.*""".r)`
    */
  def unlike (got: String, rx: Regex): Boolean =
    test(rx.findFirstIn(got).isEmpty, unlikeMessage(got, rx))

  /** Assert that a string doesn't match a regular expression.
    *
    * @example `unlike(response.header("Authorization"), """^Digest.*""".r, "we don't support digest authentication")`
    */
  def unlike (got: String, rx: Regex, desc: String): Boolean =
    testWithDesc(rx.findFirstIn(got).isEmpty, desc, unlikeMessage(got, rx))

  /** An assertion that always succeeds.
    *
    * @example `pass()`
    */
  def pass: Boolean =
    ok(true)

  /** An assertion that always succeeds, with a reason.
    *
    * @example `pass("this line of code should be executed")`
    */
  def pass (desc: String): Boolean =
    ok(true, desc)

  /** An assertion that always fails.
    *
    * @example `fail()`
    */
  def fail: Boolean =
    ok(false)

  /** An assertion that always fails, with a reason.
    *
    * @example `fail("we should never get here")`
    */
  def fail (desc: String): Boolean =
    ok(false, desc)

  /** Output a comment to `Console.err`. This is intended to be visible to
    * users even when running the test under a summarizing harness.
    *
    * @example `diag("Testing with Scala " + util.Properties.versionString)`
    */
  def diag (message: Any) {
    builder.diag(message)
  }

  /** Output a comment to `Console.out`. This is intended to only be visible
    * when viewing the raw TAP stream.
    *
    * @example `note("Starting the response tests")`
    */
  def note (message: Any) {
    builder.note(message)
  }

  /** Halt execution of the entire test suite.
    *
    * @example `BAIL_OUT("can't connect to the database!")`
    */
  def BAIL_OUT (desc: String) {
    builder.bailOut(desc)
  }

  /** Mark a block of tests as expected to fail. If the tests which run in the
    * todo block fail, they will not be treated as test failures, and if they
    * succeed, the user will be notified.
    *
    * @example `todo("waiting on fixes elsewhere") { ??? }`
    */
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

  /** Mark a block of tests that should not be run at all. They are treated as
    * always passing.
    *
    * @example `skip(3, "too dangerous to run for now") { ??? }`
    */
  def skip (count: Int, reason: String)(body: => Unit) {
    for (i <- 1 to count) {
      builder.skip(reason)
    }
  }

  /** Declare a logical group of assertions, to be run as a single test. This
    * is effectively an entirely separate test, which is run, and the result
    * of that test is reported as a single assertion in the test that contains
    * it. The subtest can specify its own plan in the same way that the
    * overall test is allowed to. The name will be used as the description for
    * the single assertion that the overall test sees.
    *
    * @example `subtest("response tests") { ??? }`
    */
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

  /** A helper method which should be used to wrap test utility methods.
    * Normally, when tests fail, a message is printed giving the file and line
    * number of the call to the test method. If you write your own test
    * methods, they will typically use the existing methods to generate
    * assertions, and so the file and line numbers will likely be much less
    * useful. Wrapping the body of your method in this method will ensure that
    * the file and line number that is reported is the line where your helper
    * method is called instead.
    *
    * @example `def testFixtures = hideTestMethod { ??? }`
    */
  def hideTestMethod[T] (body: => T): T = {
    // this just adds a method call with a known name to the stack trace, so
    // that we can detect it later
    body
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
      case Some(t) => builder.todo(t, cond, "- " + desc)
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
      case Some(t) => builder.todo(t, cond, "- " + desc)
      case None    => builder.ok(cond, "- " + desc)
    }
    if (!cond) {
      failed(Some(desc), Some(reason))
    }
    cond
  }

  private def test (cond: Boolean): Boolean = {
    todo match {
      case Some(t) => builder.todo(t, cond)
      case None    => builder.ok(cond)
    }
    if (!cond) {
      failed(None, None)
    }
    cond
  }

  private def test (cond: Boolean, reason: => String): Boolean = {
    todo match {
      case Some(t) => builder.todo(t, cond)
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
      frame.getClassName == "com.iinteractive.test.TestMore" &&
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

  protected def ignoreFrame (frame: StackTraceElement): Boolean = {
    val className = frame.getClassName
    val methodName = frame.getMethodName

    // ignore everything in this class, except the hideTestMethod call which we
    // use as a stack trace marker
    (className == "com.iinteractive.test.TestMore" &&
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
