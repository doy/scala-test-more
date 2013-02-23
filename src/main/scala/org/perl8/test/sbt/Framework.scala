package org.perl8.test.sbt

class Framework extends org.scalatools.testing.Framework {
  val name:  String                                    = "Perl8 Test"
  val tests: Array[org.scalatools.testing.Fingerprint] = Array(Fingerprint)

  def testRunner (
    testClassLoader: ClassLoader,
    loggers: Array[org.scalatools.testing.Logger]
  ): org.scalatools.testing.Runner = {
    new Runner(testClassLoader, loggers)
  }
}
