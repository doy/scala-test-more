package org.perl8.test.tap

import java.io.ByteArrayOutputStream

import org.perl8.test.{TestMore,SkipAll,BailOutException}

class TestBuilderTest extends TestMore {
  subtest ("ok") {
    val output = new ByteArrayOutputStream
    Console.withOut(output) {
      Console.withErr(output) {
        val builder = new TestBuilder(4)
        builder.ok(true, "test succeeded")
        builder.ok(false, "test failed")
        builder.ok(true)
        builder.ok(false)
        builder.doneTesting
      }
    }

    val expected =
      "1..4\n"                 +
      "ok 1 test succeeded\n"  +
      "not ok 2 test failed\n" +
      "ok 3\n"                 +
      "not ok 4\n"             +
      "# Looks like you failed 2 tests of 4.\n"

    is(output.toString, expected)
  }

  subtest ("no plan") {
    val output = new ByteArrayOutputStream
    Console.withOut(output) {
      Console.withErr(output) {
        val builder = new TestBuilder
        builder.ok(true, "test succeeded")
        builder.ok(false, "test failed")
        builder.ok(true)
        builder.ok(false)
        builder.doneTesting
      }
    }

    val expected =
      "ok 1 test succeeded\n"  +
      "not ok 2 test failed\n" +
      "ok 3\n"                 +
      "not ok 4\n"             +
      "1..4\n"                 +
      "# Looks like you failed 2 tests of 4.\n"

    is(output.toString, expected)
  }

  subtest ("empty") {
    val output = new ByteArrayOutputStream
    Console.withOut(output) {
      Console.withErr(output) {
        val builder = new TestBuilder
        builder.doneTesting
      }
    }

    val expected =
      "1..0\n" +
      "# No tests run!\n"

    is(output.toString, expected)
  }

  subtest ("diag") {
    val output = new ByteArrayOutputStream
    Console.withOut(output) {
      Console.withErr(output) {
        val builder = new TestBuilder
        builder.ok(true, "the test passes")
        builder.ok(false, "the test passes")
        builder.diag("got false, expected true")
        builder.ok(true)
        builder.diag("ending\nnow")
        builder.doneTesting
      }
    }

    val expected =
      "ok 1 the test passes\n"       +
      "not ok 2 the test passes\n"   +
      "# got false, expected true\n" +
      "ok 3\n"                       +
      "# ending\n"                   +
      "# now\n"                      +
      "1..3\n"                       +
      "# Looks like you failed 1 test of 3.\n"

    is(output.toString, expected)
  }

  subtest ("is passing") {
    val output = new ByteArrayOutputStream
    val oldOut = Console.out
    val oldErr = Console.err

    is(
      Console.withOut(output) {
        Console.withErr(output) {
          val builder = new TestBuilder
          builder.doneTesting
        }
      },
      false
    )

    is(
      Console.withOut(output) {
        Console.withErr(output) {
          val builder = new TestBuilder
          builder.ok(true)
          builder.doneTesting
        }
      },
      true
    )

    is(
      Console.withOut(output) {
        Console.withErr(output) {
          val builder = new TestBuilder
          builder.ok(true)
          builder.ok(false)
          builder.doneTesting
        }
      },
      false
    )

    is(
      Console.withOut(output) {
        Console.withErr(output) {
          val builder = new TestBuilder
          builder.ok(true)
          builder.ok(false)
          builder.ok(true)
          builder.doneTesting
        }
      },
      false
    )
  }

  subtest ("bail out") {
    val output = new ByteArrayOutputStream
    val oldOut = Console.out
    val oldErr = Console.err
    Console.withOut(output) {
      Console.withErr(output) {
        val builder = new TestBuilder
        builder.ok(true)
        try {
          builder.bailOut("oh no!")
          Console.withOut(oldOut) {
            Console.withErr(oldErr) {
              fail
            }
          }
        }
        catch {
          case e: BailOutException => Console.withOut(oldOut) {
            Console.withErr(oldErr) {
              is(e.message, "Bail out!  oh no!")
            }
          }
          case _: Throwable => Console.withOut(oldOut) {
            Console.withErr(oldErr) {
              fail
            }
          }
        }
      }
    }

    val expected =
      "ok 1\n" +
      "Bail out!  oh no!\n"

    is(output.toString, expected)
  }

  subtest ("skip all") {
    val output = new ByteArrayOutputStream
    Console.withOut(output) {
      Console.withErr(output) {
        val builder = new TestBuilder(SkipAll("foo bar"))
      }
    }

    val expected =
      "1..0 # SKIP foo bar\n"

    is(output.toString, expected)
  }

  subtest ("skip") {
    val output = new ByteArrayOutputStream
    Console.withOut(output) {
      Console.withErr(output) {
        val builder = new TestBuilder
        builder.ok(false)
        builder.skip("not now")
        builder.skip
        builder.doneTesting
      }
    }

    val expected =
      "not ok 1\n"            +
      "ok 2 # skip not now\n" +
      "ok 3 # skip\n"         +
      "1..3\n"                +
      "# Looks like you failed 1 test of 3.\n"

    is(output.toString, expected)
  }

  subtest ("todo") {
    val output = new ByteArrayOutputStream
    Console.withOut(output) {
      Console.withErr(output) {
        val builder = new TestBuilder
        builder.todo("not working yet", false, "do a thing")
        builder.todo("is it?", true)
        builder.doneTesting
      }
    }

    val expected =
      "not ok 1 do a thing # TODO not working yet\n" +
      "ok 2 # TODO is it?\n"                         +
      "1..2\n"

    is(output.toString, expected)
  }
}
