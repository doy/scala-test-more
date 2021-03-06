package com.iinteractive.test.harness

/** Classes that implement `MultiTestReporter` are capable of running a group
  * of test classes, given their names. This typically involves some sort of
  * summarization.
  *
  * @see [[com.iinteractive.test.harness.Reporter Reporter]].
  */
trait MultiTestReporter {
  /** Runs the test classes identifed by the list of fully qualified class
    * names `testNames`.
    *
    * @return The exit code for the harness to use. Will be 0 on success.
    */
  def run (testNames: Seq[String]): Int
}
