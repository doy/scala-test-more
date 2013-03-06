package com.iinteractive.test

/** Classes for TAP generation and parsing. */
package object tap {
  /** Exception representing an error during parsing. It is thrown when a TAP
    * line isn't recognized.
    */
  case class ParseException (message: String) extends RuntimeException(message)
}
