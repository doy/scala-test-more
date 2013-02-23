package org.perl8.test

import java.io.ByteArrayOutputStream

import org.perl8.test.tap.Consumer

class TestMoreTest extends TestMore {
  private object OutputContainer {
    val output = new ByteArrayOutputStream
  }

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
    pass()

    skip(2, "don't do this yet") {
      pass("skipped")
      pass()
    }

    todo("not working yet") {
      diag("fail")
      fail("it doesn't work")
      fail()
    }
  }

  Console.withOut(OutputContainer.output) {
    Console.withErr(OutputContainer.output) {
      (new MyBasicTest).run
    }
  }

  is(Consumer.parse(OutputContainer.output).exitCode, 9, "got the right plan")

  val expected =
    "# ok\n" +
    "ok 1 - it works!\n" +
    "not ok 2 - it doesn't work!\n" +
    "#   Failed test 'it doesn't work!'\n" +
    "#   at TestMoreTest.scala line 15.\n" +
    "ok 3\n" +
    "not ok 4\n" +
    "#   Failed test at TestMoreTest.scala line 17.\n" +
    "# is\n" +
    "ok 5 - it works!\n" +
    "not ok 6 - it doesn't work!\n" +
    "#   Failed test 'it doesn't work!'\n" +
    "#   at TestMoreTest.scala line 21.\n" +
    "#          got: '1'\n" +
    "#     expected: '0'\n" +
    "ok 7\n" +
    "not ok 8\n" +
    "#   Failed test at TestMoreTest.scala line 23.\n" +
    "#          got: '1'\n" +
    "#     expected: '0'\n" +
    "# isnt\n" +
    "ok 9 - it works!\n" +
    "not ok 10 - it doesn't work!\n" +
    "#   Failed test 'it doesn't work!'\n" +
    "#   at TestMoreTest.scala line 27.\n" +
    "#          got: '1'\n" +
    "#     expected: anything else\n" +
    "ok 11\n" +
    "not ok 12\n" +
    "#   Failed test at TestMoreTest.scala line 29.\n" +
    "#          got: '1'\n" +
    "#     expected: anything else\n" +
    "# like\n" +
    "ok 13 - it works!\n" +
    "not ok 14 - it doesn't work!\n" +
    "#   Failed test 'it doesn't work!'\n" +
    "#   at TestMoreTest.scala line 33.\n" +
    "#                   'foo'\n" +
    "#     doesn't match 'bar'\n" +
    "ok 15\n" +
    "not ok 16\n" +
    "#   Failed test at TestMoreTest.scala line 35.\n" +
    "#                   'foo'\n" +
    "#     doesn't match 'bar'\n" +
    "    # unlike\n" +
    "    ok 1 - it works!\n" +
    "    not ok 2 - it doesn't work!\n" +
    "    #   Failed test 'it doesn't work!'\n" +
    "    #   at TestMoreTest.scala line 40.\n" +
    "    #                   'foo'\n" +
    "    #           matches 'foo'\n" +
    "    ok 3\n" +
    "    not ok 4\n" +
    "    #   Failed test at TestMoreTest.scala line 42.\n" +
    "    #                   'foo'\n" +
    "    #           matches 'foo'\n" +
    "    1..4\n" +
    "    # Looks like you failed 2 tests of 4.\n" +
    "not ok 17 - unlikes\n" +
    "#   Failed test 'unlikes'\n" +
    "#   at TestMoreTest.scala line 37.\n" +
    "# pass\n" +
    "ok 18 - it works!\n" +
    "ok 19\n" +
    "ok 20 # skip don't do this yet\n" +
    "ok 21 # skip don't do this yet\n" +
    "# fail\n" +
    "not ok 22 - it doesn't work # TODO not working yet\n" +
    "#   Failed (TODO) test 'it doesn't work'\n" +
    "#   at TestMoreTest.scala line 56.\n" +
    "not ok 23 # TODO not working yet\n" +
    "#   Failed (TODO) test at TestMoreTest.scala line 57.\n" +
    "1..23\n" +
    "# Looks like you failed 9 tests of 23.\n"

  is(OutputContainer.output.toString, expected, "correct tap")
}
