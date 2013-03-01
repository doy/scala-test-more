package org.perl8.test.harness

/** Classes that implement `Reporter` are capable of running a test class,
  * given its name.
  *
  * See also: [[org.perl8.test.harness.MultiTestReporter]].
  */
trait Reporter {
  /** Runs the test class identifed by the fully qualified class name
    * `testName`.
    *
    * @return The exit code for the harness to use. Will be 0 on success.
    */
  def run (testName: String): Int
}
