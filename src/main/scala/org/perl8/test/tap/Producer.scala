package org.perl8.test.tap

/** Contains functions for producing individual lines of TAP.
  */
object Producer {
  import org.perl8.test.Plan

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
    result(cond, num) + " " + desc

  /** Returns a todo test result.
    *
    * @example `not ok 1 # TODO this doesn't work yet`
    */
  def todoResult (cond: Boolean, num: Int, todo: String): String =
    result(cond, num) + " # TODO " + todo

  /** Returns a todo test result that contains a description.
    *
    * @example `not ok 18 - test the feature # TODO can't figure this out`
    */
  def todoResult (cond: Boolean, num: Int, desc: String, todo: String): String =
    result(cond, num, desc) + " # TODO " + todo

  /** Returns a skipped test result.
    *
    * @example `ok 4 # skip`
    */
  def skip (num: Int): String =
    "ok " + num + " # skip"

  /** Returns a skipped test result with a reason.
    *
    * @example `ok 4 # skip this test won't run here`
    */
  def skip (num: Int, reason: String): String =
    skip(num) + " " + reason

  /** Returns a comment.
    *
    * @example `# this is a comment`
    */
  def comment (message: String): String =
    message.split("\n").map(m => "# " + m).mkString("\n")

  /** Returns a test plan.
    *
    * @example `1..5` ([[org.perl8.test.NumericPlan NumericPlan]])
    * @example `1..0 # SKIP don't run this test` ([[org.perl8.test.SkipAll SkipAll]])
    */
  def plan (plan: Plan): String =
    plan.skipAll.map(m => "1..0 # SKIP " + m).getOrElse("1.." + plan.plan)

  /** Returns a bail out.
    *
    * @example `Bail out!`
    */
  def bailOut: String =
    "Bail out!"

  /** Returns a bail out with a reason.
    *
    * @example `Bail out!  Not supported on this platform.`
    */
  def bailOut (message: String): String =
    "Bail out!  " + message
}
