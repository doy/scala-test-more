package testbuilder

import org.tap4j.model._
import org.tap4j.producer._
import org.tap4j.util._

class Builder (plan: Option[Int] = None) {
  def this (plan: Int) =
    this(Some(plan))

  def ok (test: Boolean, description: String) {
    ok(test, Some(description))
  }

  def ok (test: Boolean, description: Option[String] = None) {
    val status = if (test) StatusValues.OK else StatusValues.NOT_OK
    val result = new TestResult(status, currentTest)
    description.foreach(d => result.setDescription("- " + d))
    testSet.addTestResult(result)
    currentTest += 1
  }

  def tap: String = {
    if (noPlan) {
      testSet.setPlan(new Plan(currentTest - 1))
    }
    val tap = producer.dump(testSet)
    if (noPlan) {
      testSet.setPlan(null)
    }
    tap
  }

  private val producer = TapProducerFactory.makeTap13Producer

  private val testSet = new TestSet
  plan.foreach(p => testSet.setPlan(new Plan(p)))

  private var currentTest = 1

  private def noPlan =
    plan.isEmpty
}
