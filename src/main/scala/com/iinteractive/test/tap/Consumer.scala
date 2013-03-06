package com.iinteractive.test.tap

import com.iinteractive.test.{Plan,NumericPlan,SkipAll}

/** Contains a method to parse an individual line of TAP. */
object Consumer {
  /** Parses a line of TAP.
    *
    * @return A [[com.iinteractive.test.tap.Consumer.Line Line]] object
    *         corresponding to the parsed line.
    */
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

  /** The representation of a parsed line of TAP. */
  sealed trait Line {
    /** The meaningful portion of the TAP line. */
    def contents: String
    /** The indentation of the TAP line (used in subtests). */
    def indent:   String

    /** The line itself that was parsed. */
    override def toString: String =
      indent + contents
  }

  /** A parsed TAP line containing a comment.
    *
    * @param text The text of the comment (not including the `#`).
    */
  case class CommentLine private[Consumer] (
    text:   String,
    indent: String
  ) extends Line {
    def contents = "# " + text
  }

  /** A parsed TAP line containing a test plan.
    *
    * @param plan The [[com.iinteractive.test.Plan Plan]] that this line
    *             represents.
    */
  case class PlanLine private[Consumer] (
    plan:   Plan,
    indent: String
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

  /** A parsed TAP line containing a test result.
    *
    * @param result The [[com.iinteractive.test.tap.TestResult TestResult]]
    *               that this line represents.
    */
  case class ResultLine private[Consumer] (
    result: TestResult,
    indent: String
  ) extends Line {
    def contents = {
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
}
