package org.perl8.test

/** Base trait for test classes in this framework. Any tests that should be
  * autodiscovered by `sbt test` should extend this trait, and implement
  * [[runTests]].
  */
trait Test {
  /** Runs the test. The TAP stream will be written to Console.out and
    * Console.err, so you can swap these out as required in order to parse it.
    */
  def run: Int =
    runTests(false)

  /** Runs the test just like [[run]], but in a way that makes sense when test
   *  results are being summarized rather than directly displayed.
   *
   *  Summarizing test reporters tend to repeatedly update the same line on
   *  the terminal, so this method makes calls to
   *  [[tap.TestBuilder#diag diag]] (which sends messages to stderr, where
   *  they are typically displayed as-is) prefix the message with a newline,
   *  to ensure that the output starts on its own line.
   */
  def runInHarness: Int =
    runTests(true)

  protected def runTests (terminalInUse: Boolean): Int
}
