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
