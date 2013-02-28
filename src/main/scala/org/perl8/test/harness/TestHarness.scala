package org.perl8.test.harness

object TestHarness {
  import org.perl8.test.Test

  def main (args: Array[String]) {
    val opts = parseOpts(args.toList)
    val multi = !opts("prefer-single").asInstanceOf[Boolean]

    val exitCode = if (multi) {
      val reporterName = opts("multi-reporter").asInstanceOf[String]
      val testNames = opts("test-classes").asInstanceOf[List[String]]
      val reporter = newInstance[MultiTestReporter](reporterName)
      reporter.run(testNames)
    }
    else {
      val reporterName = opts("single-reporter").asInstanceOf[String]
      val testName = opts("test-classes").asInstanceOf[List[String]].apply(0)
      val reporter = newInstance[Reporter](reporterName)
      reporter.run(testName)
    }

    sys.exit(exitCode)
  }

  def parseOpts (args: List[String]): Map[String, Any] = args match {
    case "-r" :: singleReporter :: rest =>
      parseOpts(rest) + ("single-reporter" -> singleReporter)

    case "-R" :: multiReporter :: rest =>
      parseOpts(rest) ++ Map(
        "multi-reporter" -> multiReporter,
        "prefer-single"  -> false
      )

    case "--help" :: rest =>
      usage(0)

    case Nil => Map(
      "single-reporter" -> "org.perl8.test.harness.TAPReporter",
      "multi-reporter"  -> "org.perl8.test.harness.SummaryReporter",
      "prefer-single"   -> true,
      "test-classes"    -> Nil
    )

    case `unknownOption` =>
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

  def usage (exitCode: Int) = {
    val out = if (exitCode == 0) Console.out else Console.err
    out.println("harness [-r <single-reporter-class>]\n" +
                "        [-R <multi-reporter-class>]\n" +
                "        [--help]\n" +
                "        <test-class> [<test-class>...]\n")
    sys.exit(exitCode)
  }

  private val unknownOption = """^-.*""".r
}
