package fpspeedrun

import fpspeedrun.Ord.Compare

final case class Ratio(num: Int, denom: Int)

object Ratio {
  implicit val RatioEq: Eq[Ratio] = (x: Ratio, y: Ratio) =>
    x.num.toLong * y.denom == y.num.toLong * x.denom

  implicit val RatioOrd: Ord[Ratio] = (x: Ratio, y: Ratio) =>
    (x.num * y.denom, x.denom * y.num) match {
      case (a, b) if a == b => Compare.EQ
      case (a, b) if a > b => Compare.GT
      case (a, b) if a < b => Compare.LT
    }

  def sum(x: Ratio, y: Ratio): Ratio =
    Ratio(x.num * y.denom + x.denom * y.num, x.denom * y.denom)

  def mult(x: Ratio, y: Ratio): Ratio =
    Ratio(x.num * y.num, x.denom * y.denom)

  implicit val RatioSumSemiGroup: SemiGroup[Sum[Ratio]] = (x: Sum[Ratio], y: Sum[Ratio]) =>
    Sum(sum(x.x, y.x))

  implicit val RatioProdSemiGroup: SemiGroup[Prod[Ratio]] = (x: Prod[Ratio], y: Prod[Ratio]) =>
    Prod(mult(x.x, y.x))
}
