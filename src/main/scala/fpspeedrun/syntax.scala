package fpspeedrun

import fpspeedrun.Ord.Compare

object syntax {

  object eq {

    implicit class EqOps[T](val x: T) extends AnyVal {
      def ===(y: T)(implicit eq: Eq[T]): Boolean = eq.===(x, y)
    }

  }

  object ord {

    implicit class OrdOps[T](val x: T) extends AnyVal {
      def <>(y: T)(implicit ord: Ord[T]): Compare =
        ord.<>(x, y)
    }

  }

  object semigroup {

    implicit class SemiGroupOps[T](val x: T) extends AnyVal {
      def combine(y: T)(implicit sg: SemiGroup[T]): T =
        sg.combine(x, y)

      def |+|(y: T)(implicit sg: SemiGroup[T]): T = combine(y)
    }

  }

}
