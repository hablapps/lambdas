package lambdas
package records
package semantics

import shapeless.{ HList, Witness }

case class GenRecord[P[_], L <: HList](fields: Fields[P, L])

object GenRecord {

  trait Aux[P[_]] {
    type λ[L <: HList] = GenRecord[P, L]
  }

  class IsoRecords[P[_]](implicit Iso: IsoGen[P, HList, Record, Aux[P]#λ]) extends Records[P] {
    def record[L <: HList](fields: Fields[P, L]): P[Record[L]] =
      Iso[L].put(GenRecord(fields))

    def field[L <: HList, K](record: P[Record[L]], key: Witness.Aux[K])(
        implicit
        S: Fields.Selector[P, L, K]
    ): P[S.Out] =
      S(Iso[L].get(record).fields)
  }
}
