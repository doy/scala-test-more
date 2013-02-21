package org.perl8.test

import Utils._

object TAP {
  def result (
    cond: Boolean,
    num:  Int,
    desc: Message = NoMessage,
    todo: Message = NoMessage
  ): String =
    join(
      (if (!cond) Some("not") else None),
      Some("ok"),
      Some(num),
      desc,
      todo.map(t => "# TODO " + t)
    )

  def skip (num: Int, reason: Message = NoMessage): String =
    join(
      Some("ok"),
      Some(num),
      Some("# skip"),
      reason
    )

  def comment (message: String): String =
    message.split("\n").map(m => "# " + m).mkString("\n")

  def plan (plan: Plan): String =
    join(
      Some("1.." + plan.plan),
      (if (plan.skipAll || plan.message.isDefined) Some("#") else None),
      (if (plan.skipAll) Some("SKIP") else None),
      plan.message
    )

  def bailOut (message: Message = NoMessage) =
    join(Some("Bail out!"), message)

  private def join (strings: Option[Any]*): String =
    strings.flatMap(x => x).mkString(" ")
}
