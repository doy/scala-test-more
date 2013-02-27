package org.perl8.test.tap

import java.io.{ByteArrayInputStream,InputStream,OutputStream}
import scala.annotation.tailrec
import scala.io.Source
import scala.util.parsing.combinator._
import scala.util.parsing.input.{Position,Reader}

import org.perl8.test.Plan
import org.perl8.test.tap.Consumer._

class Parser (cb: TAPEvent => Unit) extends Parsers {
  type Elem = Line

  def this () =
    this(e => ())

  def parse (input: InputStream): TAPResult =
    tap(new LineReader(input)) match {
      case Success(result, _) => result
      case failure: NoSuccess => throw new ParseException(failure.msg)
    }

  def parse (input: String): TAPResult =
    parse(new ByteArrayInputStream(input.getBytes))

  def parse (input: OutputStream): TAPResult =
    parse(input.toString)

  def tap: Parser[TAPResult] =
    planFirst | planLast

  private def planFirst: Parser[TAPResult] =
    plan ~ rep(result) ^^ { case plan ~ results =>
      new TAPResult(plan, results)
    }

  private def planLast: Parser[TAPResult] =
    rep(result) ~ plan ^^ { case results ~ plan =>
      new TAPResult(plan, results)
    }

  private def plan: Parser[Plan] =
    planLine ^^ { _.plan }

  private def result: Parser[TestResult] =
    simpleResult | subtestResult

  private def simpleResult: Parser[TestResult] =
    resultLine ^^ { _.result }

  private def subtestResult: Parser[TestResult] =
    subtest ~ simpleResult ^^ { case subtest ~ simpleResult =>
      new TestResult(
        simpleResult.passed,
        simpleResult.number,
        simpleResult.description,
        simpleResult.directive,
        Some(subtest)
      )
    }

  private def subtest: Parser[TAPResult] = LineParser("subtest") { in =>
    val oldIndent = expectedIndent
    val newIndent = in.first.indent

    try {
      expectedIndent = newIndent
      tap(in)
    }
    finally {
      expectedIndent = oldIndent
    }
  }

  private def planLine: Parser[PlanLine] = LineParser("plan") { in =>
    val line = in.first
    if (line.indent == expectedIndent) {
      line match {
        case p: PlanLine =>
          Success(p, in.rest)
        case _ =>
          Failure("Plan line expected, but '" + line + "' found", in)
      }
    }
    else {
      Failure(
        "Plan line expected, but " +
          "'" + line + "' has incorrect indentation",
        in
      )
    }
  }

  private def resultLine: Parser[ResultLine] = LineParser("result") { in =>
    val line = in.first
    if (line.indent == expectedIndent) {
      line match {
        case p: ResultLine =>
          Success(p, in.rest)
        case _ =>
          Failure("Result line expected, but '" + line + "' found", in)
      }
    }
    else {
      Failure(
        "Result line expected, but " +
          "'" + line + "' has incorrect indentation",
        in
      )
    }
  }

  private def LineParser[T] (lineType: String)(
    body: Input => ParseResult[T]
  ): Parser[T] = Parser { in =>
    if (in.atEnd) {
      Failure(lineType + " line expected, but end of input found", in)
    }
    else {
      body(in)
    }
  }

  private class LineReader (
    in:      Stream[Char],
    lineNum: Int
  ) extends Reader[Line] {
    def this (in: InputStream) =
      this(Source.fromInputStream(in).toStream, 1)

    def atEnd: Boolean =
      nextLine.isEmpty

    def first: Line =
      nextLine.getOrElse(throw new RuntimeException("read from empty input"))

    lazy val pos =
      new LinePosition(lineNum, nextLine.map(_.toString).getOrElse(""))

    lazy val rest: Reader[Line] =
      new LineReader(remainingStream, lineNum + 1)

    private def nextLine: Option[Line] =
      state._1

    private def remainingStream: Stream[Char] =
      state._2

    private lazy val state: (Option[Line], Stream[Char]) =
      readNextLine(in)

    @tailrec
    private def readNextLine (
      stream: Stream[Char]
    ): (Option[Line], Stream[Char]) = {
      stream match {
        case Stream.Empty => (None, in)
        case s            => {
          val (line, rest) = s.span(_ != '\n') match {
            case (l, r) => (parseLine(l.mkString), r.tail)
          }
          line match {
            case _: CommentLine => readNextLine(rest)
            case other          => (Some(other), rest)
          }
        }
      }
    }
  }

  private case class LinePosition (
    override val line: Int,
    override val lineContents: String
  ) extends Position {
    def column: Int = 1
  }

  private var expectedIndent = ""
}

sealed trait TAPEvent
case class ResultEvent (result: TestResult) extends TAPEvent
case class PlanEvent (plan: Plan) extends TAPEvent
case object SubtestStartEvent extends TAPEvent
case class SubtestEndEvent (result: TestResult) extends TAPEvent
case class CommentEvent (text: String) extends TAPEvent
