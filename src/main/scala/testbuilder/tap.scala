package testbuilder

import util._

object tap {
  def result (cond: Boolean, num: Int, desc: String, todo: String): String =
    result(cond, num, Some(desc), Some(todo))

  def result (cond: Boolean, num: Int, desc: String): String =
    result(cond, num, Some(desc))

  def result (
    cond: Boolean,
    num:  Int,
    desc: Option[String] = None,
    todo: Option[String] = None
  ): String =
    join(
      (if (!cond) Some("not") else None),
      Some("ok"),
      Some(num),
      desc,
      todo.map(t => "# TODO " + t)
    )

  def skip (num: Int, reason: String): String =
    skip(num, Some(reason))

  def skip (num: Int, reason: Option[String] = None): String =
    join(
      Some("ok"),
      Some(num),
      Some("# skip"),
      reason
    )

  def comment (message: String): String =
    "# " + message

  def plan (plan: Plan): String =
    join(
      Some("1.." + plan.plan),
      (if (plan.skipAll || plan.message.isDefined) Some("#") else None),
      (if (plan.skipAll) Some("SKIP") else None),
      plan.message
    )

  def bailOut (message: String): String =
    bailOut(Some(message))

  def bailOut (message: Option[String] = None) =
    join(Some("Bail out!"), message)

  private def join (strings: Option[Any]*): String =
    strings.flatMap(x => x).mkString(" ")
}
