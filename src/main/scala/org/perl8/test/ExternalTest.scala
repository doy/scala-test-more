package org.perl8.test

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Future._
import scala.annotation.tailrec

/** Runs an external process which emits TAP, and parses it as a test.
  *
  * This test class can be used if you have existing tests that you would like
  * to be able to include in a test suite using this framework. You just need
  * to write a test class for each external test that looks like this:
  *
  * {{{
  * class MyTest1 extends ExternalTest("perl", "t/basic.t")
  * }}}
  *
  * This will run your external process, and use its TAP stream as its output.
  * This will allow it to, for instance, be a part of the test suite that runs
  * via `sbt test`. As with any other test class, its stdout and stderr will
  * be sent to `Console.out` and `Console.err`, where they can be overridden
  * as needed.
  */
class ExternalTest (cmdLine: String*) extends Test {
  protected def runTests (raw: Boolean): Int = {
    val processBuilder = new ProcessBuilder(cmdLine: _*)

    // Ensure that if stdout and stderr are both pointing to the same place (a
    // terminal or file or something) that they remain synchronized. This is
    // only possible if the endpoint they are streaming to is the same place
    // underneath, with no extra processing in between. (This is safe because
    // stdout is typically line-buffered, and we only ever output a line at a
    // time when writing TAP, so in theory, buffering of the underlying file
    // descriptor shouldn't make a difference here)
    if (Console.out eq System.out) {
      processBuilder.redirectOutput(ProcessBuilder.Redirect.INHERIT)
    }
    if (Console.err eq System.err) {
      processBuilder.redirectError(ProcessBuilder.Redirect.INHERIT)
    }

    val process = processBuilder.start

    val streams = Seq(
      Console.out -> process.getInputStream,
      Console.err -> process.getErrorStream
    )

    val listeners = streams.map { case (out, in) =>
      Future {
        val buf = new Array[Byte](1024)

        @tailrec
        def read {
          val bytes = in.read(buf)
          if (bytes >= 0) {
            out.print(new String(buf.take(bytes)))
            read
          }
        }

        read
        true
      }
    }

    val exitCode = process.waitFor
    Await.ready(Future.sequence(listeners), Duration.Inf)
    exitCode
  }
}
