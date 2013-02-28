package org.perl8.test

import java.io.ByteArrayOutputStream

import org.perl8.test.tap.Parser

class PlanTest extends TestMore {
  private class PlanTestTest extends TestMore(2) {
    is(1, 1)
    is(2, 2)
  }

  val out = new ByteArrayOutputStream
  val exitCode = Console.withOut(out) {
    Console.withErr(out) {
      (new PlanTestTest).run
    }
  }

  is((new Parser).parse(out).exitCode, 0)
  is(exitCode, 0)

  val tap =
    "1..2\n" +
    "ok 1\n" +
    "ok 2\n"

  is(out.toString, tap)
}
