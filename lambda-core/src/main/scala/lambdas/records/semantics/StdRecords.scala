package lambdas
package records
package semantics

import cats.Id
import shapeless.Witness.Aux
import shapeless.HList

object StdRecords extends Records[Id] {
  override def record[L <: HList](fields: Fields[Id, L]): Id[Record[L]] =
    GenRecord(fields)

  override def field[L <: HList, K](record: Id[Record[L]], key: Aux[K])(
      implicit S: Fields.Selector[Id, L, K]
  ): Id[S.Out] = S(record.fields)
}
