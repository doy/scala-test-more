package org.perl8.test.tap

object Producer {
  import org.perl8.test._

  def result (cond: Boolean, num: Int): String =
    (if (cond) "ok " else "not ok ") + num

  def result (cond: Boolean, num: Int, desc: String): String =
    result(cond, num) + " " + desc

  def todoResult (cond: Boolean, num: Int, todo: String): String =
    result(cond, num) + " # TODO " + todo

  def todoResult (cond: Boolean, num: Int, desc: String, todo: String): String =
    result(cond, num, desc) + " # TODO " + todo

  def skip (num: Int): String =
    "ok " + num + " # skip"

  def skip (num: Int, reason: String): String =
    skip(num) + " " + reason

  def comment (message: String): String =
    message.split("\n").map(m => "# " + m).mkString("\n")

  def plan (plan: Plan): String =
    if (plan.skipAll) {
      val skip = "1..0 # SKIP"
      plan.message.map(m => skip + " " + m).getOrElse(skip)
    }
    else {
      "1.." + plan.plan
    }

  def bailOut: String =
    "Bail out!"

  def bailOut (message: String): String =
    "Bail out!  " + message
}
