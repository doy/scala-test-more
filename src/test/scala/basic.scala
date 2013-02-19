import org.scalatest.FunSuite

import testbuilder._

class Basic extends FunSuite {
  test ("ok") {
    val builder = new Builder(4)
    builder.ok(true, "test succeeded")
    builder.ok(false, "test failed")
    builder.ok(true)
    builder.ok(false)

    val expected =
      "1..4\n"                   +
      "ok 1 - test succeeded\n"  +
      "not ok 2 - test failed\n" +
      "ok 3\n"                   +
      "not ok 4\n"

    assert(builder.tap === expected)

    builder.ok(true)
    assert(builder.tap === expected + "ok 5\n")
  }

  test ("no plan") {
    val builder = new Builder
    builder.ok(true, "test succeeded")
    builder.ok(false, "test failed")
    builder.ok(true)
    builder.ok(false)

    val expected =
      "1..4\n"                   +
      "ok 1 - test succeeded\n"  +
      "not ok 2 - test failed\n" +
      "ok 3\n"                   +
      "not ok 4\n"

    assert(builder.tap === expected)

    val expectedModified =
      "1..5\n"                   +
      "ok 1 - test succeeded\n"  +
      "not ok 2 - test failed\n" +
      "ok 3\n"                   +
      "not ok 4\n"               +
      "ok 5\n"

    builder.ok(true)
    assert(builder.tap === expectedModified)
  }

  test ("empty") {
    val builder = new Builder

    assert(builder.tap === "1..0\n")
  }

  test ("diag") {
    val builder = new Builder

    builder.ok(true, "the test passes")
    builder.ok(false, "the test passes")
    builder.diag("got false, expected true")
    builder.ok(true)

    val expected =
      "1..3\n"                       +
      "ok 1 - the test passes\n"     +
      "not ok 2 - the test passes\n" +
      "# got false, expected true\n" +
      "ok 3\n"

    assert(builder.tap === expected)
  }

  test ("is passing") {
    val builder = new Builder

    assert(!builder.isPassing)
    builder.ok(true)
    assert(builder.isPassing)
    builder.ok(false)
    assert(!builder.isPassing)
    builder.ok(true)
    assert(!builder.isPassing)
  }
}
