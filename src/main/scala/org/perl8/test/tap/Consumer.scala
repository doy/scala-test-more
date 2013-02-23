package org.perl8.test.tap

import org.perl8.test._

object Consumer {
  import scala.util.parsing.combinator._
  import java.io.OutputStream

  def parse (input: String): TAPResult = {
    import TAPParser.{parseAll,tap,Success,NoSuccess}

    parseAll(tap(), input) match {
      case Success(result, _) => result
      case failure: NoSuccess => throw new ParseException(failure.msg)
    }
  }

  def parse (input: OutputStream): TAPResult =
    parse(input.toString)

  private object TAPParser extends RegexParsers {
    def tap (indent: String = ""): Parser[TAPResult] =
      planFirst(indent) | planLast(indent)

    def planFirst (indent: String): Parser[TAPResult] =
      (line(plan, indent) ~ rep(line(result(indent), indent))) ^^ {
        case plan ~ results => new TAPResult(plan, results)
      }

    def planLast (indent: String): Parser[TAPResult] =
      (rep(line(result(indent), indent)) ~ line(plan, indent)) ^^ {
        case results ~ plan => new TAPResult(plan, results)
      }

    def comment: Parser[String] =
      """#[^\n]*""".r

    def plan: Parser[Plan] =
      (planValue <~ ws) ~ opt(planDirective) ^^ {
        case planValue ~ Some(SkipDirective(d)) => SkipAll(d)
        case planValue ~ None                   => NumericPlan(planValue)
      }

    def result (indent: String): Parser[TestResult] =
      simpleResult | subtestResult(indent)

    def simpleResult: Parser[TestResult] =
      (ok              <~ ws) ~
      (testNumber      <~ ws) ~
      (testDescription <~ ws) ~
      opt(testDirective) ^^ {
        case ok ~ testNumber ~ testDescription ~ testDirective =>
          new TestResult(ok, testNumber, testDescription, testDirective, None)
      }

    def subtestResult (indent: String): Parser[TestResult] =
      (new Parser[TAPResult] {
        def apply (in: Input) = {
          val source = in.source
          val offset = in.offset
          val str = source.subSequence(offset, source.length)
          val newIndent = ws.findPrefixMatchOf(str).getOrElse("").toString
          newIndent match {
            case "" => Failure("subtests must be indented", in)
            case _  => {
              tap(indent + newIndent)(in.drop(-indent.length))
            }
          }
        }
      } <~ indent <~ rep(comment ~ "\n" ~ indent)) ~ simpleResult ^^ {
        case tapResult ~ testResult =>
          new TestResult(
            testResult.passed,
            testResult.number,
            testResult.description,
            testResult.directive,
            Some(tapResult)
          )
      }

    def planValue: Parser[Int] =
      "1.." ~> """\d+""".r ^^ { _.toInt }

    def planDirective: Parser[Directive] =
      skipDirective

    def ok: Parser[Boolean] =
      opt("not ") <~ "ok" ^^ { _.isEmpty }

    def testNumber: Parser[Int] =
      """\d+""".r ^^ { _.toInt }

    def testDescription: Parser[String] =
      """[^#\n]*""".r ^^ { _.trim }

    def testDirective: Parser[Directive] =
      todoDirective | skipDirective

    def skipDirective: Parser[Directive] =
      "#" ~> ws ~> """(?i:skip)""".r ~> ws ~> opt("""[^\n]*""".r) ^^ {
        case desc => new SkipDirective(desc.map(s => s.trim))
      }

    def todoDirective: Parser[Directive] =
      "#" ~> ws ~> """(?i:todo)""".r ~> ws ~> opt("""[^\n]*""".r) ^^ {
        case desc => new TodoDirective(desc.map(s => s.trim))
      }

    def line[T] (p: => Parser[T], indent: String): Parser[T] =
      rep(indent ~ comment ~ "\n") ~>
      indent ~> p <~ "\n" <~
      rep(indent ~ comment ~ "\n")

    override def skipWhitespace = false

    val ws = """[ \t]*""".r
  }
}

trait Directive {
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

case class ParseException (
  val message: String
) extends RuntimeException(message)
