package org.perl8.test

import java.io.ByteArrayOutputStream

trait NumberZero { this: TestMore =>
  def is_zero (i: Int, desc: String): Boolean = {
    withLevel(1) {
      is(i, 0, desc)
    }
  }
}

trait NumberZeroWrapped extends NumberZero { this: TestMore =>
  def isZero (i: Int): Boolean = {
    withLevel(1) {
      is_zero(i, "the number is zero")
    }
  }
}

class ExtensionTest extends TestMore {
  private class ExtensionTestTest extends TestMore with NumberZeroWrapped {
    is_zero(0, "it's zero")
    is_zero(1, "it's not zero")
    isZero(0)
    isZero(1)
  }

  val out = new ByteArrayOutputStream
  val exitCode = Console.withOut(out) {
    Console.withErr(out) {
      (new ExtensionTestTest).run
    }
  }

  is(exitCode, 2)

  val tap =
    "ok 1 - it's zero\n" +
    "not ok 2 - it's not zero\n" +
    "#   Failed test 'it's not zero'\n" +
    "#   at ExtensionTest.scala line 24.\n" +
    "#          got: '1'\n" +
    "#     expected: '0'\n" +
    "ok 3 - the number is zero\n" +
    "not ok 4 - the number is zero\n" +
    "#   Failed test 'the number is zero'\n" +
    "#   at ExtensionTest.scala line 26.\n" +
    "#          got: '1'\n" +
    "#     expected: '0'\n" +
    "1..4\n" +
    "# Looks like you failed 2 tests of 4.\n"

  is(out.toString, tap)
}
