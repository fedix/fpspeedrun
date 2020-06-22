package fpspeedrun

import fpspeedrun.syntax.eq._

trait Eq[T] {
  def ===(x: T, y: T): Boolean
}

object Eq {
  implicit def compareList[T: Eq](implicit eq: Eq[T]): Eq[List[T]] =
    (x, y) => x.size == y.size && x.zip(y).forall {
      case (x, y) => eq.===(x, y)
    }

  object Laws {
    def reflectivity[T: Eq](x: T, y: T): Boolean =
      x === x

    def symmetry[T: Eq](x: T, y: T): Boolean =
      (x === y) == (y === x)

    def transitivity[T: Eq](x: T, y: T, z: T): Boolean =
      !((x === y) && (y === z) || (x === z))
  }

}
