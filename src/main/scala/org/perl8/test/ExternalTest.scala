package org.perl8.test

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Future._
import scala.annotation.tailrec

class ExternalTest (cmdLine: Seq[String]) extends Test {
  def run: Int = {
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
