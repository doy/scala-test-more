package com.iinteractive.test

import java.io.ByteArrayOutputStream

import com.iinteractive.test.tap.Parser

class FormattingTest extends TestMore {
  val lineZero = Thread.currentThread.getStackTrace()(1).getLineNumber + 3
  def line (offset: Int) = lineZero + offset

  private class FormattingTestTest extends TestMore {
    is(1, 1, "newlines\nwork")
    is(1, 2, "newlines\nwork")

    diag("newlines\nwork")
    note("newlines\nwork")

    is(1, 1, "\nnewlines\n")
    is(1, 2, "\nnewlines\n")

    diag("\nnewlines\n")
    note("\nnewlines\n")
  }

  val out = new ByteArrayOutputStream
  val exitCode = Console.withOut(out) {
    Console.withErr(out) {
      (new FormattingTestTest).run
    }
  }

  is((new Parser).parse(out).exitCode, 2)
  is(exitCode, 2)

  val tap =
    "ok 1 - newlines\n" +
    "# work\n" +
    "not ok 2 - newlines\n" +
    "# work\n" +
    "#   Failed test 'newlines\n" +
    "# work'\n" +
    "#   at FormattingTest.scala line " + line(2) + ".\n" +
    "#          got: '1'\n" +
    "#     expected: '2'\n" +
    "# newlines\n" +
    "# work\n" +
    "# newlines\n" +
    "# work\n" +
    "ok 3 - \n" +
    "# newlines\n" +
    "# \n" +
    "not ok 4 - \n" +
    "# newlines\n" +
    "# \n" +
    "#   Failed test '\n" +
    "# newlines\n" +
    "# '\n" +
    "#   at FormattingTest.scala line " + line(8) + ".\n" +
    "#          got: '1'\n" +
    "#     expected: '2'\n" +
    "# \n" +
    "# newlines\n" +
    "# \n" +
    "# newlines\n" +
    "1..4\n" +
    "# Looks like you failed 2 tests of 4.\n"

  is(out.toString, tap)
}
