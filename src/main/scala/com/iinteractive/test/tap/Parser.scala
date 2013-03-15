package com.iinteractive.test.tap

import java.io.{ByteArrayInputStream,InputStream,OutputStream}
import scala.annotation.tailrec
import scala.io.Source
import scala.util.parsing.combinator._
import scala.util.parsing.input.{Position,Reader}

import com.iinteractive.test.Plan
import com.iinteractive.test.tap.Consumer._

/** This class parses a TAP stream. It can either parse it all at once (from a
  * string), or it can be used as a streaming parser, where TAP events are
  * emitted through a given callback.
  */
class Parser private (
  cb:     TAPEvent => Unit,
  indent: String
) {
  /** Creates a parser instance.
    * @param cb The event handler callback. It will be called after each
    *           meaningful line of TAP, with a
    *           [[com.iinteractive.test.tap.TAPEvent TAPEvent]] instance
    *           representing the event that was just parsed.
    */
  def this (cb: TAPEvent => Unit = e => ()) =
    this(cb, "")

  private def this (indent: String) =
    this(e => (), indent)

  /** Parses TAP from an input stream. This variant will actually parse lines
    * as they are available to read from the input stream, so this can be used
    * as a streaming parser.
    */
  def parse (input: InputStream): TAPResult = {
    import parser._

    cb(StartEvent)
    tap(new LineReader(input)) match {
      case Success(result, _) => {
        cb(EndEvent(result))
        result
      }
      case failure: NoSuccess => throw new ParseException(failure.msg)
    }
  }

  /** Parses TAP contained in a string. This isn't useful for incremental
    * parsing, because the entire input string must be created before
    * parsing can begin.
    */
  def parse (input: String): TAPResult =
    parse(new ByteArrayInputStream(input.getBytes))

  /** Parses TAP from an output stream.
    *
    * @todo Doesn't currently work as a streaming parser, since it just
    *       collects the entire output as a string and feeds it to the parser
    *       for strings. This could likely be improved.
    */
  def parse (input: OutputStream): TAPResult =
    parse(input.toString)

  private val parser = new TAPParser(cb, indent)

  private class TAPParser (
    cb:     TAPEvent => Unit,
    indent: String
  ) extends Parsers {
    type Elem = Line

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
      planLine ^^ { p =>
        cb(PlanEvent(p.plan))
        p.plan
      }

    private def result: Parser[TestResult] =
      simpleResult | subtestResult

    private def simpleResult: Parser[TestResult] =
      resultLine ^^ { r =>
        cb(ResultEvent(r.result))
        r.result
      }

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

    private def subtest: Parser[TAPResult] =
      LineParser("subtest") { in =>
        // can't just return the result directly, because it's of a different
        // type (the path dependent type associated with the new Parser
        // instance we create here, rather than the path dependent type
        // associated with this)
        val subParser = new TAPParser(
          e => (),
          in.first.indent
        )
        subParser.tap(in) match {
          case subParser.Success(p, rest) => Success(p, rest)
          case subParser.Failure(m, rest) => Failure(m, rest)
          case subParser.Error(m, rest)   => Error(m, rest)
        }
      }

    private def planLine: Parser[PlanLine] = LineParser("plan") { in =>
      val line = in.first
      if (line.indent == indent) {
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
      if (line.indent == indent) {
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
  }

  private class LineReader (
    in:      Iterator[Char],
    lineNum: Int
  ) extends Reader[Line] {
    def this (in: InputStream) =
      this(Source.fromInputStream(in), 1)

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

    private def remainingStream: Iterator[Char] =
      state._2

    private lazy val state: (Option[Line], Iterator[Char]) =
      readNextLine(in)

    @tailrec
    private def readNextLine (
      stream: Iterator[Char]
    ): (Option[Line], Iterator[Char]) = {
      if (stream.hasNext) {
        val (line, rest) = stream.span(_ != '\n') match {
          case (l, r) => (parseLine(l.mkString), r.drop(1))
        }
        cb(LineEvent(line))
        line match {
          case _: CommentLine => readNextLine(rest)
          case other          => (Some(other), rest)
        }
      }
      else {
        (None, in)
      }
    }
  }

  private case class LinePosition (
    line: Int,
    lineContents: String
  ) extends Position {
    def column: Int = 1
  }
}
