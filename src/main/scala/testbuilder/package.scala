package object testbuilder {
  import language.implicitConversions

  implicit def intToPlan (p: Int): Plan =
    new NumericPlan(p)

  type Plan        = util.Plan
  type NumericPlan = util.NumericPlan
  type SkipAll     = util.SkipAll
  val SkipAll      = util.SkipAll

  type BailOutException = util.BailOutException
}
