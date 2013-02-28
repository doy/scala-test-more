package org.perl8.test.tap

import org.perl8.test.{Plan,NumericPlan,SkipAll}

object Consumer {
  def parseLine (line: String): Line = {
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

  sealed trait Line {
    def contents: String
    def indent: String
    override def toString: String =
      indent + contents
  }

  case class CommentLine (
    val text:            String,
    override val indent: String
  ) extends Line {
    def contents = "# " + text
  }

  case class PlanLine (
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

  case class ResultLine(
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

  private val commentRx = """^(\s*)#\s*(.*)""".r
  private val planRx    = """^(\s*)1..(\d+)\s*(?:# SKIP (.*))?""".r
  private val resultRx  =
    """^(\s*)(not )?ok (\d+)\s*([^#]+)?(?:#\s*(?i:(skip|todo))\s+(.*))?""".r

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
      case _              => results.length == 0
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
}
