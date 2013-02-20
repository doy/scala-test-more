package object testbuilder {
  import language.implicitConversions

  implicit def intToPlan (p: Int): Plan =
    new NumericPlan(p)

  implicit def stringToMessage (s: String): util.Message =
    new util.HasMessage(s)

  type Plan        = util.Plan
  type NumericPlan = util.NumericPlan
  type SkipAll     = util.SkipAll
  val SkipAll      = util.SkipAll

  type BailOutException = util.BailOutException
}
