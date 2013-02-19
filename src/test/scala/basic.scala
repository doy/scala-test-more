import org.scalatest.FunSuite

import testbuilder._

import java.io.ByteArrayOutputStream

class Basic extends FunSuite {
  test ("ok") {
    val output = new ByteArrayOutputStream
    val builder = new Builder(4, output)
    builder.ok(true, "test succeeded")
    builder.ok(false, "test failed")
    builder.ok(true)
    builder.ok(false)
    builder.doneTesting

    val expected =
      "1..4\n"                 +
      "ok 1 test succeeded\n"  +
      "not ok 2 test failed\n" +
      "ok 3\n"                 +
      "not ok 4\n"

    assert(output.toString === expected)
  }

  test ("no plan") {
    val output = new ByteArrayOutputStream
    val builder = new Builder(output)
    builder.ok(true, "test succeeded")
    builder.ok(false, "test failed")
    builder.ok(true)
    builder.ok(false)
    builder.doneTesting

    val expected =
      "ok 1 test succeeded\n"  +
      "not ok 2 test failed\n" +
      "ok 3\n"                 +
      "not ok 4\n"             +
      "1..4\n"

    assert(output.toString === expected)
  }

  test ("empty") {
    val output = new ByteArrayOutputStream
    val builder = new Builder(output)
    builder.doneTesting

    assert(output.toString === "1..0\n")
  }

  test ("diag") {
    val output = new ByteArrayOutputStream
    val builder = new Builder(output)

    builder.ok(true, "the test passes")
    builder.ok(false, "the test passes")
    builder.diag("got false, expected true")
    builder.ok(true)
    builder.doneTesting

    val expected =
      "ok 1 the test passes\n"       +
      "not ok 2 the test passes\n"   +
      "# got false, expected true\n" +
      "ok 3\n"                       +
      "1..3\n"

    assert(output.toString === expected)
  }

  test ("is passing") {
    val output = new ByteArrayOutputStream
    val builder = new Builder(output)

    assert(!builder.isPassing)
    builder.ok(true)
    assert(builder.isPassing)
    builder.ok(false)
    assert(!builder.isPassing)
    builder.ok(true)
    assert(!builder.isPassing)
  }
}
