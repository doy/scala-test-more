package org.perl8

package object test {
  import language.implicitConversions

  implicit def intToPlan (p: Int): Plan =
    new NumericPlan(p)

  implicit def stringToMessage (s: String): Utils.Message =
    new Utils.HasMessage(s)

  type Plan        = Utils.Plan
  type NumericPlan = Utils.NumericPlan
  type SkipAll     = Utils.SkipAll
  val SkipAll      = Utils.SkipAll

  type BailOutException = Utils.BailOutException
}
