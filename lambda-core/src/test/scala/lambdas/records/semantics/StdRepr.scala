package lambdas
package records
package semantics

import lambdas.arithmetic.Num
import shapeless.HList
import tfhoas.semantics.GenProducts._

sealed abstract class StdRepr[_]
case class IntStd(i: Num)                                  extends StdRepr[Num]
case class RecordStd[L <: HList](r: GenRecord[StdRepr, L]) extends StdRepr[Record[L]]
case class Tuple2Std[A, B](r: (StdRepr[A], StdRepr[B]))    extends StdRepr[(A, B)]
case class Arrow1Std[A, B](r: StdRepr[A] => StdRepr[B])    extends StdRepr[A => B]

object StdRepr {
  type Aux[T <: HList] = GenRecord[StdRepr, T]

  implicit val IsoStdRepr = new IsoGen[StdRepr, HList, Record, Aux] {
    def apply[L <: HList] = Iso({ case RecordStd(repr) => repr }, RecordStd.apply)
  }

  implicit val Product2IsoGen = new IsoGen2[StdRepr, Tuple2, GenTuple2[StdRepr, ?, ?]] {
    def apply[T1, T2] = Iso({ case Tuple2Std(repr) => repr }, Tuple2Std.apply)
  }

  implicit val Arrow1IsoGen = new IsoGen2[StdRepr, Function1, GenArrow1[StdRepr, ?, ?]] {
    def apply[T1, T2] = Iso({ case Arrow1Std(repr) => repr }, Arrow1Std.apply)
  }

  implicit val SetterStdRepr = new SetterGen[StdRepr, Num, Num] {
    def apply() = Setter(IntStd.apply)
  }
}
