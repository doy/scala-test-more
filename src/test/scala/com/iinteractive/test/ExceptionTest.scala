package com.iinteractive.test

import java.io.ByteArrayOutputStream

import com.iinteractive.test.tap.Parser

class ExceptionTest extends TestMore {
  val lineZero = Thread.currentThread.getStackTrace()(1).getLineNumber + 3
  def line (offset: Int) = lineZero + offset

  private class MyBasicTest extends TestMore {
    private case class MyException(msg: String)
      extends RuntimeException(msg)
    private case class OtherException(msg: String)
      extends RuntimeException(msg)

    lives_ok {
      val _ = "no exception"
    }
    lives_ok {
      throw MyException("foo")
    }

    dies_ok[MyException] {
      val _ = "no exception"
    }
    dies_ok[MyException] {
      throw OtherException("foo")
    }
    dies_ok[MyException] {
      throw MyException("foo")
    }

    throws_ok(MyException("foo")) {
      val _ = "no exception"
    }
    throws_ok(MyException("foo")) {
      throw MyException("bar")
    }
    throws_ok(MyException("foo")) {
      throw MyException("foo")
    }

    is(
      exception { val _ = "no exception" },
      None
    )
    is(
      exception { throw MyException("foo") },
      Some(MyException("foo"))
    )
  }

  val out = new ByteArrayOutputStream
  val exitCode = Console.withOut(out) {
    Console.withErr(out) {
      (new MyBasicTest).run
    }
  }

  is((new Parser).parse(out).exitCode, 5, "got the right plan")
  is(exitCode, 5, "got the right plan")

  val expected =
    ("""^ok 1 - didn't throw an exception
not ok 2 - didn't throw an exception
#   Failed test 'didn't throw an exception'
#   at ExceptionTest.scala line """ + line(9) + """.
#          got: [^\s]*MyException[^\n]*foo[^\n]*
#     expected: normal exit
not ok 3 - threw a [^\s]*MyException exception
#   Failed test 'threw a [^\s]*MyException exception'
#   at ExceptionTest.scala line """ + line(13) + """.
#          got: normal exit
#     expected: a [^\s]*MyException exception
not ok 4 - threw a [^\s]*MyException exception
#   Failed test 'threw a [^\s]*MyException exception'
#   at ExceptionTest.scala line """ + line(16) + """.
#          got: [^\s]*OtherException[^\n]*foo[^\n]*
#     expected: a [^\s]*MyException exception
ok 5 - threw a [^\s]*MyException exception
not ok 6 - threw [^\s]*MyException[^\n]*foo[^\n]*
#   Failed test 'threw [^\s]*MyException[^\n]*foo[^\n]*'
#   at ExceptionTest.scala line """ + line(23) + """.
#          got: normal exit
#     expected: [^\s]*MyException[^\n]*foo[^\n]*
not ok 7 - threw [^\s]*MyException[^\n]*foo[^\n]*
#   Failed test 'threw [^\s]*MyException[^\n]*foo[^\n]*'
#   at ExceptionTest.scala line """ + line(26) + """.
#          got: [^\s]*MyException[^\n]*bar[^\n]*
#     expected: [^\s]*MyException[^\n]*foo[^\n]*
ok 8 - threw [^\s]*MyException[^\n]*foo[^\n]*
ok 9
ok 10
1..10
# Looks like you failed 5 tests of 10.
$""").r

  like(out.toString, expected, "correct tap")
}
