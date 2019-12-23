package lambdas
package records
package syntax

import shapeless.labelled.FieldType
import shapeless.{ ::, HList, HNil, Witness }

trait FieldsSyntax {

  class FieldsBuilder[P[_], L <: HList](tail: Fields[P, L]) {
    def ::[K, V](head: FieldType[K, P[V]])(
        implicit
        W: Witness.Aux[K]
    ): FieldsBuilder[P, FieldType[K, V] :: L] =
      new FieldsBuilder(HConsFields(W, head, tail))

    def returns: Fields[P, L] = tail
  }

  implicit def toBuilder[P[_], K, V](head: FieldType[K, P[V]])(
      implicit
      W: Witness.Aux[K]
  ): FieldsBuilder[P, FieldType[K, V] :: HNil] =
    new FieldsBuilder(HConsFields(W, head, NilFields[P]()))

  implicit def toFields[P[_], L <: HList](b: FieldsBuilder[P, L]): Fields[P, L] =
    b.returns

  implicit def toSingleFields[P[_], K, V](head: FieldType[K, P[V]])(
      implicit
      W: Witness.Aux[K]
  ): Fields[P, FieldType[K, V] :: HNil] =
    HConsFields(W, head, NilFields[P]())

  implicit def productToFields[P[_], A <: Product](a: A)(
      implicit
      PF: ProductToFields[A, P]
  ): Fields[P, PF.Out] =
    PF(a)
}
