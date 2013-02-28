package org.perl8.test.harness

import java.io.{PipedInputStream,PipedOutputStream}
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import org.perl8.test.tap.Consumer.{TAPResult,TodoDirective}
import org.perl8.test.tap._
import org.perl8.test.Test

trait SummarizedTests {
  def runOneTest (test: Test, cb: TAPEvent => Unit): TAPResult = {
    val out = new PipedOutputStream
    val in  = new PipedInputStream(out)

    val testFuture = Future {
      Console.withOut(out) {
        test.runInHarness
      }
      out.close
    }

    val parser = new Parser(cb)
    val result = parser.parse(in)
    in.close
    Await.ready(testFuture, Duration.Inf)

    result
  }
}
