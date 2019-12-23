package lambdas
package records
package semantics

import cats.~>
import shapeless.{ HList, Witness }
import tfdb.semantics.OpenHOAS

class ForallOpenHOAS[P[_]](implicit R: Records[P]) extends Forall[OpenHOAS[P, ?, ?], Records] {
  def apply[E] = new Records[OpenHOAS[P, E, ?]] {

    def record[L <: HList](fields: Fields[OpenHOAS[P, E, ?], L]): OpenHOAS[P, E, Record[L]] =
      OpenHOAS { env =>
        R.record(fields(λ[OpenHOAS[P, E, ?] ~> P] { _(env) }))
      }

    def field[L <: HList, K](record: OpenHOAS[P, E, Record[L]], key: Witness.Aux[K])(
        implicit
        S: Fields.Selector[OpenHOAS[P, E, ?], L, K]
    ): OpenHOAS[P, E, S.Out] =
      OpenHOAS { env =>
        R.field(record(env), key)(new Fields.Selector[P, L, K] {
          type Out = S.Out
          def apply(fields: Fields[P, L]): P[Out] =
            S(fields(λ[P ~> OpenHOAS[P, E, ?]] { p =>
              OpenHOAS { _ =>
                p
              }
            }))(env)
        })
      }
  }
}
