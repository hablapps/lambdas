package lambdas
package records
package syntax

import cats.Id
import semantics.Record
import shapeless.{ HList, HNil, Witness }

trait RecordsSyntax {

  implicit class ProductOps[A <: Product](a: A) {
    def record[P[_]](implicit PF: ProductToFields[A, P], R: Records[P]): P[Record[PF.Out]] =
      R.record(PF(a))
  }

  implicit class HListOps[L <: HList](a: L) {
    def record[P[_]](implicit LF: HListToFields[L, P], R: Records[P]): P[Record[L]] =
      R.record(LF(a))
  }

  def nilR[P[_]](implicit R: Records[P]): P[Record[HNil]] =
    R.record(NilFields[P]())

  def Record[L <: HList](l: L)(implicit LF: HListToFields[L, Id], R: Records[Id]): Record[L] =
    l.record[Id]

  def Record[A <: Product](
      a: A
  )(implicit PF: ProductToFields[A, Id], R: Records[Id]): Record[PF.Out] =
    a.record[Id]

  implicit class GenRecordOps[L <: HList, P[_]](r: P[Record[L]])(implicit R: Records[P]) {
    def field[K](k: Witness.Aux[K])(implicit S: Fields.Selector[P, L, K]): P[S.Out] =
      R.field(r, k)
  }

}
