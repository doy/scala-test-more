package org.perl8.test.tap

import org.scalatest.FunSuite

import org.perl8.test.Utils._

class ConsumerTest extends FunSuite {
  test ("basic") {
    val tap =
      "1..1\n" +
      "ok 1\n"

    val result = Consumer.parse(tap)
    assert(result.plan === NumericPlan(1))
    assert(result.results.map(_.passed) === Seq(true))
  }

  test ("skip all") {
    val tap =
      "1..0 # SKIP nope\n"

    val result = Consumer.parse(tap)
    assert(result.plan === SkipAll("nope"))
    assert(result.results === Seq())
  }

  test ("more complicated") {
    val tap =
      "# starting...\n" +
      "ok 1 - stuff\n" +
      "not ok 2 - does this work?\n" +
      "not ok 3 - eventually # TODO doesn't work yet\n" +
      "# skipping some stuff\n" +
      "ok 4 # skip don't do this yet\n" +
      "# finished!\n" +
      "1..4\n" +
      "# Looks like you failed 1 test of 4.\n"

    val result = Consumer.parse(tap)
    assert(result.plan === NumericPlan(4))
    assert(result.results.map(_.passed) === Seq(true, false, false, true))
    assert(result.results.map(_.number) === Seq(1, 2, 3, 4))
    assert(
      result.results.map(_.description) === Seq(
        "- stuff",
        "- does this work?",
        "- eventually",
        ""
      )
    )
    assert(
      result.results.map(_.directive) === Seq(
        None,
        None,
        Some(TodoDirective(Some("doesn't work yet"))),
        Some(SkipDirective(Some("don't do this yet")))
      )
    )
  }

  test ("subtests") {
    val tap =
      "ok 1 - not subtest\n" +
      "    ok 1 - passed\n" +
      "    not ok 2 - failed\n" +
      "    ok 3 - passed again\n" +
      "        1..1\n" +
      "        ok 1 - sub-sub-test\n" +
      "    ok 4 - nested subtests\n" +
      "    1..4\n" +
      "    # Looks like you failed 1 test of 4.\n" +
      "not ok 2 - subtest\n" +
      "1..2\n" +
      "# Looks like you failed 1 test of 2.\n"

    val result = Consumer.parse(tap)
    assert(result.plan === NumericPlan(2))
    assert(result.results.map(_.passed) === Seq(true, false))
    assert(result.results.map(_.number) === Seq(1, 2))
    assert(
      result.results.map(_.description) === Seq(
        "- not subtest",
        "- subtest"
      )
    )
    assert(result.results.map(_.directive) === Seq(None, None))

    assert(result.results(0).subtest === None)
    assert(result.results(1).subtest.isDefined)

    val subtest = result.results(1).subtest.get
    assert(subtest.plan === NumericPlan(4))
    assert(subtest.results.map(_.passed) === Seq(true, false, true, true))
    assert(subtest.results.map(_.number) === Seq(1, 2, 3, 4))
    assert(
      subtest.results.map(_.description) === Seq(
        "- passed",
        "- failed",
        "- passed again",
        "- nested subtests"
      )
    )
    assert(subtest.results.map(_.directive) === Seq(None, None, None, None))

    assert(subtest.results(0).subtest === None)
    assert(subtest.results(1).subtest === None)
    assert(subtest.results(2).subtest === None)
    assert(subtest.results(3).subtest.isDefined)

    val subsubtest = subtest.results(3).subtest.get
    assert(subsubtest.plan === NumericPlan(1))
    assert(subsubtest.results.map(_.passed) === Seq(true))
    assert(subsubtest.results.map(_.number) === Seq(1))
    assert(
      subsubtest.results.map(_.description) === Seq(
        "- sub-sub-test"
      )
    )
    assert(subsubtest.results.map(_.directive) === Seq(None))

    assert(subsubtest.results(0).subtest === None)
  }
}
