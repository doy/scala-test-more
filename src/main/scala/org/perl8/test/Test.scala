package org.perl8.test

/** Base trait for test classes in this framework
  */
trait Test {
  /** Runs the test. The TAP stream will be written to Console.out and
    * Console.err, so you can swap these out as required in order to parse it.
    */
  def run (): Int
}
