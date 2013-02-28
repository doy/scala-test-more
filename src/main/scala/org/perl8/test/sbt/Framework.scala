package org.perl8.test.sbt

import org.scalatools.testing

class Framework extends testing.Framework {
  val name:  String                     = "Perl8 Test"
  val tests: Array[testing.Fingerprint] = Array(Fingerprint)

  def testRunner (
    testClassLoader: ClassLoader,
    loggers:         Array[testing.Logger]
  ): testing.Runner = {
    new Runner(testClassLoader, loggers)
  }
}
