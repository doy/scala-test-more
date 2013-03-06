package com.iinteractive.test.sbt

import org.scalatools.testing

/** Implementation of
  * [[http://github.com/harrah/test-interface/blob/master/src/org/scalatools/testing/Fingerprint.java org.scalatools.testing.Fingerprint]].
  */
object Fingerprint extends testing.SubclassFingerprint {
  def isModule:       Boolean = false
  def superClassName: String  = "com.iinteractive.test.Test"
}
