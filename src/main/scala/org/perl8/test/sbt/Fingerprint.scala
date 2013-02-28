package org.perl8.test.sbt

import org.scalatools.testing

object Fingerprint extends testing.SubclassFingerprint {
  def isModule:       Boolean = false
  def superClassName: String  = "org.perl8.test.Test"
}
