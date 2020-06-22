package fpspeedrun

import fpspeedrun.Ord.Compare
import fpspeedrun.Ord.Compare._


trait Ord[T] extends Eq[T] {
  def compare(x: T, y: T): Compare

  override def ===(x: T, y: T): Boolean =
    compare(x, y) == EQ

  def >(x: T, y: T): Boolean =
    compare(x, y) == GT

  def <(x: T, y: T): Boolean =
    compare(x, y) == LT

  def <>(x: T, y: T): Compare = compare(x, y)
}

object Ord {

  sealed trait Compare

  object Compare {

    case object LT extends Compare

    case object EQ extends Compare

    case object GT extends Compare

  }

  implicit def listOrdCompare[T: Ord](implicit ord: Ord[T]): Ord[List[T]] =
    (x, y) => x.zip(y).collectFirst {
      case (x, y) if ord.<>(x, y) != EQ => ord.<>(x, y)
    }.getOrElse {
      (x.size, y.size) match {
        case (n, k) if n > k => GT
        case (n, k) if n < k => LT
        case (n, k) if n == k => EQ
      }
    }
}