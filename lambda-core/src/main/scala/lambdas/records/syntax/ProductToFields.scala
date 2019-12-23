package lambdas.records.syntax

import lambdas.records.Fields
import shapeless.{ HList, LabelledGeneric }

trait ProductToFields[A <: Product, P[_]] {
  type Out <: HList
  def apply(a: A): Fields[P, Out]
}

object ProductToFields {

  type Aux[A <: Product, Repr[_], Out0] = ProductToFields[A, Repr] { type Out = Out0 }

  implicit def inst[A <: Product, Repr[_], L <: HList](
      implicit
      lg: LabelledGeneric.Aux[A, L],
      toFields: HListToFields[L, Repr]
  ): ProductToFields.Aux[A, Repr, L] =
    new ProductToFields[A, Repr] {
      type Out = L
      override def apply(a: A): Fields[Repr, L] =
        toFields(lg.to(a))
    }
}
