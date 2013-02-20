package org.perl8.test.tests.testmore

import org.scalatest.FunSuite

import org.perl8.test.TestMore

import java.io.ByteArrayOutputStream

object OutputContainer {
  val output = new ByteArrayOutputStream
}

class MyBasicTest extends TestMore(OutputContainer.output) {
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

class Basic extends FunSuite {
  test ("basic") {
    assert((new MyBasicTest).run == 9)

    val expected =
      "# ok\n" +
      "ok 1 - it works!\n" +
      "not ok 2 - it doesn't work!\n" +
      "#   Failed test 'it doesn't work!'\n" +
      "#   at basic.scala line 16.\n" +
      "ok 3\n" +
      "not ok 4\n" +
      "#   Failed test at basic.scala line 18.\n" +
      "# is\n" +
      "ok 5 - it works!\n" +
      "not ok 6 - it doesn't work!\n" +
      "#   Failed test 'it doesn't work!'\n" +
      "#   at basic.scala line 22.\n" +
      "#          got: '1'\n" +
      "#     expected: '0'\n" +
      "ok 7\n" +
      "not ok 8\n" +
      "#   Failed test at basic.scala line 24.\n" +
      "#          got: '1'\n" +
      "#     expected: '0'\n" +
      "# isnt\n" +
      "ok 9 - it works!\n" +
      "not ok 10 - it doesn't work!\n" +
      "#   Failed test 'it doesn't work!'\n" +
      "#   at basic.scala line 28.\n" +
      "#          got: '1'\n" +
      "#     expected: anything else\n" +
      "ok 11\n" +
      "not ok 12\n" +
      "#   Failed test at basic.scala line 30.\n" +
      "#          got: '1'\n" +
      "#     expected: anything else\n" +
      "# like\n" +
      "ok 13 - it works!\n" +
      "not ok 14 - it doesn't work!\n" +
      "#   Failed test 'it doesn't work!'\n" +
      "#   at basic.scala line 34.\n" +
      "#                   'foo'\n" +
      "#     doesn't match 'bar'\n" +
      "ok 15\n" +
      "not ok 16\n" +
      "#   Failed test at basic.scala line 36.\n" +
      "#                   'foo'\n" +
      "#     doesn't match 'bar'\n" +
      "    # unlike\n" +
      "    ok 1 - it works!\n" +
      "    not ok 2 - it doesn't work!\n" +
      "    #   Failed test 'it doesn't work!'\n" +
      "    #   at basic.scala line 41.\n" +
      "    #                   'foo'\n" +
      "    #           matches 'foo'\n" +
      "    ok 3\n" +
      "    not ok 4\n" +
      "    #   Failed test at basic.scala line 43.\n" +
      "    #                   'foo'\n" +
      "    #           matches 'foo'\n" +
      "    1..4\n" +
      "    # Looks like you failed 2 tests of 4.\n" +
      "not ok 17 - unlikes\n" +
      "#   Failed test 'unlikes'\n" +
      "#   at basic.scala line 38.\n" +
      "# pass\n" +
      "ok 18 - it works!\n" +
      "ok 19\n" +
      "ok 20 # skip don't do this yet\n" +
      "ok 21 # skip don't do this yet\n" +
      "# fail\n" +
      "not ok 22 - it doesn't work # TODO not working yet\n" +
      "#   Failed (TODO) test 'it doesn't work'\n" +
      "#   at basic.scala line 57.\n" +
      "not ok 23 # TODO not working yet\n" +
      "#   Failed (TODO) test at basic.scala line 58.\n" +
      "1..23\n" +
      "# Looks like you failed 9 tests of 23.\n"

    assert(OutputContainer.output.toString === expected)
  }
}
