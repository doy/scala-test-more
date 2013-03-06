package com.iinteractive.test.tap

import com.iinteractive.test.{TestMore,SkipAll,NumericPlan}

class ParserTest extends TestMore {
  subtest ("basic") {
    val tap =
      "1..1\n" +
      "ok 1\n"

    val result = (new Parser).parse(tap)
    is(result.plan, NumericPlan(1), "got the right plan")
    is(result.results.map(_.passed), Seq(true), "got the right results")
  }

  subtest ("skip all") {
    val tap =
      "1..0 # SKIP nope\n"

    val result = (new Parser).parse(tap)
    is(result.plan, SkipAll("nope"), "got the right plan")
    is(result.results, Nil, "got the right results")
  }

  subtest ("more complicated") {
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

    val result = (new Parser).parse(tap)
    is(result.plan, NumericPlan(4))
    is(result.results.map(_.passed), Seq(true, false, false, true))
    is(result.results.map(_.number), Seq(1, 2, 3, 4))
    is(
      result.results.map(_.description),
      Seq(
        "- stuff",
        "- does this work?",
        "- eventually",
        ""
      )
    )

    is(
      result.results.map(_.directive),
      Seq(
        None,
        None,
        Some(TodoDirective(Some("doesn't work yet"))),
        Some(SkipDirective(Some("don't do this yet")))
      )
    )
  }

  subtest ("subtests") {
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

    val result = (new Parser).parse(tap)
    is(result.plan, NumericPlan(2))
    is(result.results.map(_.passed), Seq(true, false))
    is(result.results.map(_.number), Seq(1, 2))
    is(
      result.results.map(_.description),
      Seq(
        "- not subtest",
        "- subtest"
      )
    )
    is(result.results.map(_.directive), Seq(None, None))

    is(result.results(0).subtest, None)
    ok(result.results(1).subtest.isDefined)

    val subtest = result.results(1).subtest.get
    is(subtest.plan, NumericPlan(4))
    is(subtest.results.map(_.passed), Seq(true, false, true, true))
    is(subtest.results.map(_.number), Seq(1, 2, 3, 4))
    is(
      subtest.results.map(_.description),
      Seq(
        "- passed",
        "- failed",
        "- passed again",
        "- nested subtests"
      )
    )
    is(subtest.results.map(_.directive), Seq(None, None, None, None))

    is(subtest.results(0).subtest, None)
    is(subtest.results(1).subtest, None)
    is(subtest.results(2).subtest, None)
    ok(subtest.results(3).subtest.isDefined)

    val subsubtest = subtest.results(3).subtest.get
    is(subsubtest.plan, NumericPlan(1))
    is(subsubtest.results.map(_.passed), Seq(true))
    is(subsubtest.results.map(_.number), Seq(1))
    is(
      subsubtest.results.map(_.description),
      Seq(
        "- sub-sub-test"
      )
    )
    is(subsubtest.results.map(_.directive), Seq(None))

    is(subsubtest.results(0).subtest, None)
  }
}
