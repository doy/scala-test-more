package com.iinteractive.test.harness

import java.io.{PipedInputStream,PipedOutputStream}
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import com.iinteractive.test.tap.{Parser,TAPEvent,TAPResult,TodoDirective}
import com.iinteractive.test.Test

/** This is a trait for classes that run tests and summarize the results. It
  * provides a single `runOneTest` method, which runs a test class and
  * produces a stream of [[com.iinteractive.test.tap.TAPEvent TAP events]]
  * which can be used to produce whatever summarized output you need.
  */
trait SummarizedTests {
  /** Runs a single [[com.iinteractive.test.Test test]] instance, calling `cb`
    * with each [[com.iinteractive.test.tap.TAPEvent TAP event]] as it is
    * produced.
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
