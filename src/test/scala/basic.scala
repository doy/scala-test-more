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
      "not ok 4\n"             +
      "# Looks like you failed 2 tests of 4.\n"

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
      "1..4\n"                 +
      "# Looks like you failed 2 tests of 4.\n"

    assert(output.toString === expected)
  }

  test ("empty") {
    val output = new ByteArrayOutputStream
    val builder = new Builder(output)
    builder.doneTesting

    val expected =
      "1..0\n" +
      "# No tests run!\n"

    assert(output.toString === expected)
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
      "1..3\n"                       +
      "# Looks like you failed 1 test of 3.\n"

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

  test ("bail out") {
    val output = new ByteArrayOutputStream
    val builder = new Builder(output)

    builder.ok(true)
    val e = intercept[BailOutException] {
      builder.bailOut("oh no!")
    }
    assert(e.message === "oh no!")

    val expected =
      "ok 1\n" +
      "Bail out! oh no!\n"

    assert(output.toString === expected)
  }

  test ("skip all") {
    val output = new ByteArrayOutputStream
    val builder = new Builder(SkipAll(), output)

    val expected =
      "1..0 # SKIP\n"

    assert(output.toString === expected)
  }

  test ("skip all with reason") {
    val output = new ByteArrayOutputStream
    val builder = new Builder(SkipAll("foo bar"), output)

    val expected =
      "1..0 # SKIP foo bar\n"

    assert(output.toString === expected)
  }

  test ("skip") {
    val output = new ByteArrayOutputStream
    val builder = new Builder(output)

    builder.ok(false)
    builder.skip("not now")
    builder.skip()
    builder.doneTesting

    val expected =
      "not ok 1\n"            +
      "ok 2 # skip not now\n" +
      "ok 3 # skip\n"         +
      "1..3\n"                +
      "# Looks like you failed 1 test of 3.\n"

    assert(output.toString === expected)
  }
}
