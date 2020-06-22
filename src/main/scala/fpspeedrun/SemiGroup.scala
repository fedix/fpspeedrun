package fpspeedrun

import syntax.semigroup._

trait SemiGroup[T] {
  def combine(x: T, y: T): T
}

object SemiGroup {

  object Laws {
    def associativity[T: SemiGroup](x: T, y: T, z: T): Boolean =
      ((x |+| y) |+| z) == (x |+| (y |+| z))
  }

  implicit val stringSemiGroup: SemiGroup[String] = (x: String, y: String) => x + y

  def combineList[T: SemiGroup](list: List[T]): Option[T] =
    list.reduceOption((a, b) => a |+| b)

  def combineListVia[U: SemiGroup, T](list: List[T])(implicit iso: Iso[T, U]): Option[T] =
    list.reduceOption((x, y) => iso.unwrap(iso.wrap(x) |+| iso.wrap(y)))

}

final case class Sum[T](x: T) extends AnyVal
final case class Prod[T](x: T) extends AnyVal
