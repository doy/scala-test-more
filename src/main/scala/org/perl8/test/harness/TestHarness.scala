package org.perl8.test.harness

import org.perl8.test.Test

object TestHarness {
  def main (args: Array[String]) {
    val className = args(0)
    val loader = classOf[Test].getClassLoader
    val test = loader.loadClass(className).newInstance.asInstanceOf[Test]
    val exitCode = test.run
    sys.exit(exitCode)
  }
}
