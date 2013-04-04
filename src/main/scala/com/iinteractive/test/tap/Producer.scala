package com.iinteractive.test.tap

/** Contains functions for producing individual lines of TAP. */
object Producer {
  import com.iinteractive.test.Plan

  /** Returns a test result.
    *
    * @example `ok 4`
    */
  def result (cond: Boolean, num: Int): String =
    (if (cond) "ok " else "not ok ") + num

  /** Returns a test result that contains a description.
    *
    * @example `ok 28 - our test succeeded`
    */
  def result (cond: Boolean, num: Int, desc: String): String =
    result(cond, num) + " " + sanitize(desc)

  /** Returns a todo test result.
    *
    * @example `not ok 1 # TODO this doesn't work yet`
    */
  def todoResult (cond: Boolean, num: Int, todo: String): String =
    result(cond, num) + " # TODO " + sanitize(todo)

  /** Returns a todo test result that contains a description.
    *
    * @example `not ok 18 - test the feature # TODO can't figure this out`
    */
  def todoResult (cond: Boolean, num: Int, desc: String, todo: String): String =
    result(cond, num, desc) + " # TODO " + sanitize(todo)

  /** Returns a skipped test result with a reason.
    *
    * @example `ok 4 # skip this test won't run here`
    */
  def skip (num: Int, reason: String): String =
    "ok " + num + " # skip " + sanitize(reason)

  /** Returns a comment.
    *
    * @example `# this is a comment`
    */
  def comment (message: String): String =
    "# " + sanitize(if (message.last == '\n') message.init else message)

  /** Returns a test plan.
    *
    * @example `1..5` ([[com.iinteractive.test.NumericPlan NumericPlan]])
    * @example `1..0 # SKIP don't run this test` ([[com.iinteractive.test.SkipAll SkipAll]])
    */
  def plan (plan: Plan): String =
    plan.skipAll.map(
      m => "1..0 # SKIP " + sanitize(m)
    ).getOrElse("1.." + plan.plan)

  /** Returns a bail out with a reason.
    *
    * @example `Bail out!  Not supported on this platform.`
    */
  def bailOut (message: String): String =
    "Bail out!  " + message

  private def sanitize (text: String): String =
    text.replace("\n", "\n# ")
}
