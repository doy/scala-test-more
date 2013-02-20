import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

import testbuilder._

import java.io.ByteArrayOutputStream

class Basic extends FunSuite with BeforeAndAfter {
  private val output = new ByteArrayOutputStream

  before {
    output.reset
  }

  test ("ok") {
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
    val builder = new Builder(output)
    builder.doneTesting

    val expected =
      "1..0\n" +
      "# No tests run!\n"

    assert(output.toString === expected)
  }

  test ("diag") {
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
    val builder = new Builder(SkipAll(), output)

    val expected =
      "1..0 # SKIP\n"

    assert(output.toString === expected)
  }

  test ("skip all with reason") {
    val builder = new Builder(SkipAll("foo bar"), output)

    val expected =
      "1..0 # SKIP foo bar\n"

    assert(output.toString === expected)
  }

  test ("skip") {
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

  test ("todo") {
    val builder = new Builder(output)

    builder.ok(false, "do a thing", "not working yet")
    builder.ok(true, "some other thing", "is it?")
    builder.doneTesting

    val expected =
      "not ok 1 do a thing # TODO not working yet\n" +
      "ok 2 some other thing # TODO is it?\n"        +
      "1..2\n"

    assert(output.toString === expected)
  }
}
