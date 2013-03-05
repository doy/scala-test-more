package com.iinteractive.test.sbt

import org.scalatools.testing

/** Implementation of
  * [[http://github.com/harrah/test-interface/blob/master/src/org/scalatools/testing/Framework.java org.scalatools.testing.Framework]].
  */
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
