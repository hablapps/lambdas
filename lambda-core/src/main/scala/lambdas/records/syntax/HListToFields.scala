package lambdas
package records
package syntax

import shapeless._
import shapeless.labelled._

sealed abstract class HListToFields[L <: HList, Repr[_]] {
  def apply(hlist: L): Fields[Repr, L]
}

object HListToFields {

  implicit def NilHListToFields[Repr[_]]: HListToFields[HNil, Repr] =
    new HListToFields[HNil, Repr] {
      def apply(hlist: HNil) = NilFields()
    }

  implicit def HConsHListToFields[Repr[_], K, H, T <: HList](
      implicit
      W: Witness.Aux[K],
      S: SetterGen[Repr, H, H],
      T: HListToFields[T, Repr]
  ): HListToFields[FieldType[K, H] :: T, Repr] =
    new HListToFields[FieldType[K, H] :: T, Repr] {
      def apply(hlist: FieldType[K, H] :: T) =
        HConsFields[Repr, K, H, T](W, field[K](S().put(hlist.head)), T(hlist.tail))
    }
}
