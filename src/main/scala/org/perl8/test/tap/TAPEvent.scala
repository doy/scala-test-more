package org.perl8.test.tap

import org.perl8.test.Plan

/** An event emitted while parsing a TAP stream. */
sealed trait TAPEvent

/** The start of a TAP stream. */
case object StartEvent extends TAPEvent

/** The end of a TAP stream.
  * @param result The [[org.perl8.test.tap.TAPResult TAPResult]] containing
  *               information about all of the tests which just finished
  *               running. This will be the same thing that is returned by the
  *               call to [[org.perl8.test.tap.Parser Parser]]'s `parse`
  *               method.
  */
case class  EndEvent private[tap] (result: TAPResult) extends TAPEvent

/** An individual test result.
  * @param result The [[org.perl8.test.tap.TestResult TestResult]] containing
  *               information about the corresponding test.
  */
case class  ResultEvent private[tap] (result: TestResult) extends TAPEvent

/** A test plan.
 * @param plan The [[org.perl8.test.Plan Plan]] corresponding to the line that
 *             was parsed.
 */
case class  PlanEvent private[tap] (plan: Plan) extends TAPEvent

/** The start of a subtest (currently unused). */
case object SubtestStartEvent extends TAPEvent

/** The end of a subtest (currently unused). */
case class  SubtestEndEvent private[tap] (result: TestResult) extends TAPEvent

/** A comment (currently unused). */
case class  CommentEvent private[tap] (text: String) extends TAPEvent
