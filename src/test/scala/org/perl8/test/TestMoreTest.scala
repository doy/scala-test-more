package org.perl8.test

import java.io.ByteArrayOutputStream

import org.perl8.test.tap.Parser

class TestMoreTest extends TestMore {
  val lineZero = Thread.currentThread.getStackTrace()(1).getLineNumber + 3
  def line (offset: Int) = lineZero + offset

  private class MyBasicTest extends TestMore {
    diag("ok")
    ok(1 == 1, "it works!")
    ok(0 == 1, "it doesn't work!")
    ok(1 == 1)
    ok(0 == 1)

    diag("is")
    is(1, 1, "it works!")
    is(1, 0, "it doesn't work!")
    is(1, 1)
    is(1, 0)

    diag("isnt")
    isnt(1, 0, "it works!")
    isnt(1, 1, "it doesn't work!")
    isnt(1, 0)
    isnt(1, 1)

    diag("like")
    like("foo", """foo""".r, "it works!")
    like("foo", """bar""".r, "it doesn't work!")
    like("foo", """foo""".r)
    like("foo", """bar""".r)

    subtest("unlikes") {
      diag("unlike")
      unlike("foo", """bar""".r, "it works!")
      unlike("foo", """foo""".r, "it doesn't work!")
      unlike("foo", """bar""".r)
      unlike("foo", """foo""".r)
    }

    diag("pass")
    pass("it works!")
    pass

    skip(2, "don't do this yet") {
      pass("skipped")
      pass
    }

    todo("not working yet") {
      diag("fail")
      fail("it doesn't work")
      fail
    }
  }

  val out = new ByteArrayOutputStream
  val exitCode = Console.withOut(out) {
    Console.withErr(out) {
      (new MyBasicTest).run
    }
  }

  is((new Parser).parse(out).exitCode, 9, "got the right plan")
  is(exitCode, 9, "got the right plan")

  val expected =
    "# ok\n" +
    "ok 1 - it works!\n" +
    "not ok 2 - it doesn't work!\n" +
    "#   Failed test 'it doesn't work!'\n" +
    "#   at TestMoreTest.scala line " + line(3) + ".\n" +
    "ok 3\n" +
    "not ok 4\n" +
    "#   Failed test at TestMoreTest.scala line " + line(5) + ".\n" +
    "# is\n" +
    "ok 5 - it works!\n" +
    "not ok 6 - it doesn't work!\n" +
    "#   Failed test 'it doesn't work!'\n" +
    "#   at TestMoreTest.scala line " + line(9) + ".\n" +
    "#          got: '1'\n" +
    "#     expected: '0'\n" +
    "ok 7\n" +
    "not ok 8\n" +
    "#   Failed test at TestMoreTest.scala line " + line(11) + ".\n" +
    "#          got: '1'\n" +
    "#     expected: '0'\n" +
    "# isnt\n" +
    "ok 9 - it works!\n" +
    "not ok 10 - it doesn't work!\n" +
    "#   Failed test 'it doesn't work!'\n" +
    "#   at TestMoreTest.scala line " + line(15) + ".\n" +
    "#          got: '1'\n" +
    "#     expected: anything else\n" +
    "ok 11\n" +
    "not ok 12\n" +
    "#   Failed test at TestMoreTest.scala line " + line(17) + ".\n" +
    "#          got: '1'\n" +
    "#     expected: anything else\n" +
    "# like\n" +
    "ok 13 - it works!\n" +
    "not ok 14 - it doesn't work!\n" +
    "#   Failed test 'it doesn't work!'\n" +
    "#   at TestMoreTest.scala line " + line(21) + ".\n" +
    "#                   'foo'\n" +
    "#     doesn't match 'bar'\n" +
    "ok 15\n" +
    "not ok 16\n" +
    "#   Failed test at TestMoreTest.scala line " + line(23) + ".\n" +
    "#                   'foo'\n" +
    "#     doesn't match 'bar'\n" +
    "    # unlike\n" +
    "    ok 1 - it works!\n" +
    "    not ok 2 - it doesn't work!\n" +
    "    #   Failed test 'it doesn't work!'\n" +
    "    #   at TestMoreTest.scala line " + line(28) + ".\n" +
    "    #                   'foo'\n" +
    "    #           matches 'foo'\n" +
    "    ok 3\n" +
    "    not ok 4\n" +
    "    #   Failed test at TestMoreTest.scala line " + line(30) + ".\n" +
    "    #                   'foo'\n" +
    "    #           matches 'foo'\n" +
    "    1..4\n" +
    "    # Looks like you failed 2 tests of 4.\n" +
    "not ok 17 - unlikes\n" +
    "#   Failed test 'unlikes'\n" +
    "#   at TestMoreTest.scala line " + line(25) + ".\n" +
    "# pass\n" +
    "ok 18 - it works!\n" +
    "ok 19\n" +
    "ok 20 # skip don't do this yet\n" +
    "ok 21 # skip don't do this yet\n" +
    "# fail\n" +
    "not ok 22 - it doesn't work # TODO not working yet\n" +
    "#   Failed (TODO) test 'it doesn't work'\n" +
    "#   at TestMoreTest.scala line " + line(44) + ".\n" +
    "not ok 23 # TODO not working yet\n" +
    "#   Failed (TODO) test at TestMoreTest.scala line " + line(45) + ".\n" +
    "1..23\n" +
    "# Looks like you failed 9 tests of 23.\n"

  is(out.toString, expected, "correct tap")
}
