package com.iinteractive.test.harness

import java.io.{PipedInputStream,PipedOutputStream}
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Try,Success,Failure}

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
  protected def runOneTest (
    test:    Test,
    cb:      TAPEvent => Unit,
    combine: Boolean = false
  ): TAPResult = {
    val out = new PipedOutputStream
    val in  = new PipedInputStream(out)
    val err = if (combine) out else Console.err

    val testFuture = Future {
      val result = Try {
        Console.withOut(out) {
          Console.withErr(err) {
            if (combine) {
              test.run
            }
            else {
              test.runInHarness
            }
          }
        }
      }
      out.close
      result
    }

    val parser = new Parser(cb)
    val result = Try(parser.parse(in))
    in.close
    Await.result(testFuture, Duration.Inf) match {
      case Success(_) => ()
      case Failure(e) => throw e
    }

    result match {
      case Success(r) => r
      case Failure(e) => throw e
    }
  }
}
