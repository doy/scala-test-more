package org.perl8.test

package object tap {
  case class ParseException (message: String) extends RuntimeException(message)
}
