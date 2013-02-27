package org.perl8.test.tap

import java.io.{ByteArrayInputStream,InputStream,OutputStream}
import scala.annotation.tailrec
import scala.io.Source
import scala.util.parsing.combinator._
import scala.util.parsing.input.{Position,Reader}

import org.perl8.test.{Plan,NumericPlan,SkipAll}

object Consumer {
  def parse (input: InputStream, cb: TAPEvent => Unit): TAPResult =
    consumer(cb).parse(input)

  def parse (input: InputStream): TAPResult =
    consumer().parse(input)

  def parse (input: String, cb: TAPEvent => Unit): TAPResult =
    consumer(cb).parse(new ByteArrayInputStream(input.getBytes))

  def parse (input: String): TAPResult =
    consumer().parse(new ByteArrayInputStream(input.getBytes))

  // XXX should be able to make a streaming input stream out of this
  def parse (input: OutputStream, cb: TAPEvent => Unit): TAPResult =
    consumer(cb).parse(new ByteArrayInputStream(input.toString.getBytes))

  def parse (input: OutputStream): TAPResult =
    consumer().parse(new ByteArrayInputStream(input.toString.getBytes))

  private def consumer (cb: TAPEvent => Unit = e => ()) =
    new Consumer(cb)
}

class Consumer (cb: TAPEvent => Unit) {
  def parse (input: InputStream): TAPResult = {
    import TAPParser.{tap,Success,NoSuccess}

    tap(new LineReader(input)) match {
      case Success(result, _) => result
      case failure: NoSuccess => throw new ParseException(failure.msg)
    }
  }

  private object TAPParser extends Parsers {
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

    private def subtest: Parser[TAPResult] = Parser { in =>
      if (in.atEnd) {
        Failure("Subtest expected, but end of input found", in)
      }
      else {
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
    }

    private def planLine: Parser[PlanLine] = Parser { in =>
      if (in.atEnd) {
        Failure("Plan line expected, but end of input found", in)
      }
      else {
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
    }

    private def resultLine: Parser[ResultLine] = Parser { in =>
      if (in.atEnd) {
        Failure("Result line expected, but end of input found", in)
      }
      else {
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
    }

    private var expectedIndent = ""
  }

  private sealed trait Line {
    def contents: String
    def indent: String
    override def toString: String =
      indent + contents
  }

  private object Line {
    def apply (line: String): Line = {
      commentRx.findFirstMatchIn(line).map { m =>
        m.subgroups match {
          case Seq(indent, text) => new CommentLine(text, indent)
        }
      }.getOrElse {
        planRx.findFirstMatchIn(line).map { m =>
          m.subgroups match {
            case Seq(indent, p, null) =>
              new PlanLine(NumericPlan(p.toInt), indent)
            case Seq(indent, _, skip) =>
              new PlanLine(SkipAll(skip), indent)
          }
        }.getOrElse {
          resultRx.findFirstMatchIn(line).map { m =>
            val indent = m.group(1)
            val passed = m.group(2) == null
            val number = m.group(3).toInt
            val description = m.group(4) match {
              case null => ""
              case s    => s.trim
            }
            val directive = (m.group(5), m.group(6)) match {
              case (null, null) => None
              case (d, r)       => {
                val reason = if (r == null) "" else r
                """(?i:skip)""".r.findFirstIn(d) match {
                  case Some(_) => Some(new SkipDirective(Some(reason)))
                  case None    => Some(new TodoDirective(Some(reason)))
                }
              }
            }
            val result = new TestResult(
              passed,
              number,
              description,
              directive,
              None
            )
            new ResultLine(result, indent)
          }.getOrElse {
            throw ParseException("Couldn't parse line: " + line)
          }
        }
      }
    }

    private val commentRx = """^(\s*)#\s*(.*)""".r
    private val planRx    = """^(\s*)1..(\d+)\s*(?:# SKIP (.*))?""".r
    private val resultRx  =
      """^(\s*)(not )?ok (\d+)\s*([^#]+)?(?:#\s*(?i:(skip|todo))\s+(.*))?""".r
  }

  private case class CommentLine (
    val text:            String,
    override val indent: String
  ) extends Line {
    def contents = "# " + text
  }

  private case class PlanLine (
    val plan:            Plan,
    override val indent: String
  ) extends Line {
    def contents = {
      val count = plan.plan
      val comment = plan match {
        case SkipAll(m) => " # SKIP " + m
        case _          => ""
      }
      indent + "1.." + count + comment
    }
  }

  private case class ResultLine(
    val result:          TestResult,
    override val indent: String
  ) extends Line {
    def contents =  {
      val success = (if (result.passed) "ok" else "not ok") + " "
      val number = result.number + " "
      val description = result.description match {
        case "" => ""
        case s  => s + " "
      }
      val directive = result.directive.map { d =>
        d match {
          case TodoDirective(m) => "# TODO " + m
          case SkipDirective(m) => "# skip " + m
        }
      }.getOrElse("")
      indent + success + number + description + directive
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

    def rest: Reader[Line] =
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
            case (l, r) => (Line(l.mkString), r.tail)
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
}

sealed trait Directive {
  val message: Option[String]
}
case class SkipDirective (
  override val message: Option[String]
) extends Directive
case class TodoDirective (
  override val message: Option[String]
) extends Directive

case class TestResult (
  val passed:      Boolean,
  val number:      Int,
  val description: String,
  val directive:   Option[Directive],
  val subtest:     Option[TAPResult]
)

class TAPResult (val plan: Plan, val results: Seq[TestResult]) {
  val correctPlan = plan match {
    case NumericPlan(n) => results.length == n
    case SkipAll(_)     => results.length == 0
  }

  val fails = results.count { r =>
    !r.passed && !r.directive.isDefined
  }

  val testsPassed = fails == 0

  val success =
    correctPlan && testsPassed

  val exitCode =
    if (success) {
      0
    }
    else if (!correctPlan) {
      255
    }
    else {
      fails
    }
}

sealed trait TAPEvent
case class ResultEvent (result: TestResult) extends TAPEvent
case class PlanEvent (plan: Plan) extends TAPEvent
case object SubtestStartEvent extends TAPEvent
case class SubtestEndEvent (result: TestResult) extends TAPEvent
case class CommentEvent (text: String) extends TAPEvent

case class ParseException (
  val message: String
) extends RuntimeException(message)
