import fpspeedrun.{Eq, Ord, Prod, Ratio, SemiGroup, Sum}
import fpspeedrun.syntax.eq._
import fpspeedrun.syntax.ord._
import fpspeedrun.syntax.semigroup._


val list1 = Ratio(1, 2) :: Ratio(2, 4) :: Nil
val list2 = Ratio(1, 2) :: Ratio(3, 6) :: Nil

// Eq
Ratio(2, 4) === Ratio(1, 2)
Ratio(1, 2) === Ratio(1, 2)
Ratio(3, 4) === Ratio(1, 2)

list1 === list2


// Ord
Ratio.RatioOrd.compare(Ratio(1, 2), Ratio(2, 4))
Ratio(1, 2) <> Ratio(3, 4)
list1 <> list2


// Ratio operations
val r1 = Ratio(1, 2)
val r2 = Ratio(2, 5)
Ratio.sum(r1, r2)
Ratio.mult(r1, r2)

list1.reduceOption(Ratio.sum)


// SemiGroup
"hello" |+| " semigroup"
val listStr = List("a", "b", "c")
SemiGroup.combineList(listStr)

Sum(r1) |+| Sum(r2)
SemiGroup.combineList(list1.map(Sum(_))).map(_.x)
SemiGroup.combineList(list1.map(Prod(_))).map(_.x)
