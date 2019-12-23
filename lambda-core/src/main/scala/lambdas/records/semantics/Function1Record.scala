package lambdas
package records
package semantics

import shapeless._
import cats.~>

object Function1Records extends Forall[Function1, Records] {
  def apply[E] = new Records[Function1[E, ?]] {
    def record[L <: HList](fields: Fields[Function1[E, ?], L]): Function1[E, Record[L]] =
      env => GenRecord(fields(λ[(E => ?) ~> Id](_(env))))

    def field[L <: HList, K](record: Function1[E, Record[L]], key: Witness.Aux[K])(
        implicit
        S: Fields.Selector[Function1[E, ?], L, K]
    ): Function1[E, S.Out] =
      env => S(record(env).fields(λ[Id ~> (E => ?)](a => (_ => a))))(env)
  }
}
