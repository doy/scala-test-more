package testbuilder

import util._

object tap {
  def result (cond: Boolean, num: Int, desc: String): String =
    result(cond, num, Some(desc))

  def result (cond: Boolean, num: Int, desc: Option[String] = None): String =
    Seq(
      if (cond) Some("ok") else Some("not ok"),
      Some(num),
      desc
    ).flatMap(x => x).mkString(" ")

  def comment (message: String): String =
    "# " + message

  def plan (plan: Plan): String =
    Seq(
      Some("1.." + plan.plan),
      (if (plan.skipAll || plan.message.isDefined) Some("#") else None),
      (if (plan.skipAll) Some("SKIP") else None),
      plan.message
    ).flatMap(x => x).mkString(" ")

  def bailOut (message: String): String =
    bailOut(Some(message))

  def bailOut (message: Option[String] = None) =
    Seq(Some("Bail out!"), message).flatMap(x => x).mkString(" ")
}
