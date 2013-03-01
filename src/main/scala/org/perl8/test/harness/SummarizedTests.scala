package org.perl8.test.harness

import java.io.{PipedInputStream,PipedOutputStream}
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import org.perl8.test.tap.{Parser,TAPEvent,TAPResult,TodoDirective}
import org.perl8.test.Test

/** This is a trait for classes that run tests and summarize the results. It
  * provides a single `runOneTest` method, which runs a test class and
  * produces a stream of [[org.perl8.test.tap.TAPEvent TAP events]] which can
  * be used to produce whatever summarized output you need.
  */
trait SummarizedTests {
  /** Runs a single [[org.perl8.test.Test test]] instance, calling `cb` with
    * each [[org.perl8.test.tap.TAPEvent TAP event]] as it is produced.
    *
    * @return The overall result of the test instance.
    */
  protected def runOneTest (test: Test, cb: TAPEvent => Unit): TAPResult = {
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
