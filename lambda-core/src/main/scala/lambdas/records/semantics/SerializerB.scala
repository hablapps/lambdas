package lambdas
package records
package semantics

import shapeless.{ HList, Witness }
import trees._, TreeSerializable.{ ShowTree, ShowTreeB }

object SerializerB extends Forall[ShowTreeB, Records] {
  def apply[E] = new Records[ShowTreeB[E, ?]] {

    def record[L <: HList](fields: Fields[ShowTreeB[E, ?], L]): ShowTreeB[E, Record[L]] =
      Records[ShowTree].record(fields)

    def field[L <: HList, K](record: ShowTreeB[E, Record[L]], key: Witness.Aux[K])(
        implicit
        S: Fields.Selector[ShowTreeB[E, ?], L, K]
    ): ShowTreeB[E, S.Out] =
      Records[ShowTree].field(record, key)
  }
}
