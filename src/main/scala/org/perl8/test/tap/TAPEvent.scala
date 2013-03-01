package org.perl8.test.tap

import org.perl8.test.Plan

sealed trait TAPEvent
case object StartEvent extends TAPEvent
case class  EndEvent (result: TAPResult) extends TAPEvent
case class  ResultEvent (result: TestResult) extends TAPEvent
case class  PlanEvent (plan: Plan) extends TAPEvent
case object SubtestStartEvent extends TAPEvent
case class  SubtestEndEvent (result: TestResult) extends TAPEvent
case class  CommentEvent (text: String) extends TAPEvent
