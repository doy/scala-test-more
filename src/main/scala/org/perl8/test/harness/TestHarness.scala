package org.perl8.test.harness

/** This is the entry point to running tests written with this library from
  * the command line. Note that this library also implements the
  * [[https://github.com/harrah/test-interface common testing interface]] for
  * test libraries, so tests should also just work with `sbt test`.
  *
  * If this application is run and given just a single test class name, it
  * will run that test and write its TAP stream to the console.
  *
  * {{{
  * $ scala org.perl8.test.harness.TestHarness MyTest
  * ok 1
  * ok 2
  * 1..2
  * }}}
  *
  * If this application is run and given multiple test class names, it will
  * run each of those tests, and present a summary report, similar to the one
  * produces by
  * [[https://metacpan.org/module/Test::Harness Perl's Test::Harness]].
  *
  * {{{
  * $ scala org.perl8.test.harness.TestHarness MyTest1 MyTest2
  * MyTest1 .. ok
  * MyTest2 .. ok
  * All tests successful.
  * Files=2, Tests=4
  * Result: PASS
  * }}}
  *
  * This application also accepts a few command line options to customize its
  * behavior:
  *
  *  - `-r`: Alternative [[org.perl8.test.harness.Reporter Reporter]] class to
  *          use for running a single test.
  *  - `-R`: Alternative
  *          [[org.perl8.test.harness.MultiTestReporter MultiTestReporter]]
  *          class to use for running a group of tests. Also enables using the
  *          MultiTestReporter for a single test.
  *  - `--help`: Prints usage information.
  */
object TestHarness {
  import org.perl8.test.Test

  /** Entry point for the harness application. */
  def main (args: Array[String]) {
    val opts = parseOpts(args.toList)
    val single = opts("prefer-single").asInstanceOf[Boolean]

    val exitCode = if (single) {
      val reporterName = opts("single-reporter").asInstanceOf[String]
      val testName = opts("test-classes").asInstanceOf[List[String]].apply(0)
      val reporter = newInstance[Reporter](reporterName)
      reporter.run(testName)
    }
    else {
      val reporterName = opts("multi-reporter").asInstanceOf[String]
      val testNames = opts("test-classes").asInstanceOf[List[String]]
      val reporter = newInstance[MultiTestReporter](reporterName)
      reporter.run(testNames)
    }

    sys.exit(exitCode)
  }

  protected def parseOpts (args: List[String]): Map[String, Any] = args match {
    case Nil => Map(
      "single-reporter" -> "org.perl8.test.harness.TAPReporter",
      "multi-reporter"  -> "org.perl8.test.harness.SummaryReporter",
      "prefer-single"   -> true,
      "test-classes"    -> Nil
    )

    case "-r" :: singleReporter :: rest =>
      parseOpts(rest) + ("single-reporter" -> singleReporter)

    case "-R" :: multiReporter :: rest =>
      parseOpts(rest) ++ Map(
        "multi-reporter" -> multiReporter,
        "prefer-single"  -> false
      )

    case "--help" :: rest =>
      usage(0)

    case `unknownOption` :: rest =>
      usage(1)

    case testClass :: rest => {
      val opts = parseOpts(rest)
      val tests = opts("test-classes").asInstanceOf[List[String]]
      opts ++ Map(
        "test-classes"  -> (testClass :: tests),
        "prefer-single" -> tests.isEmpty
      )
    }
  }

  protected def usage (exitCode: Int) = {
    val out = if (exitCode == 0) Console.out else Console.err
    out.println("harness [-r <single-reporter-class>]\n" +
                "        [-R <multi-reporter-class>]\n" +
                "        [--help]\n" +
                "        <test-class> [<test-class>...]\n")
    sys.exit(exitCode)
  }

  private val unknownOption = """^-.*""".r
}
