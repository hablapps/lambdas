package lambdas
package strings
package semantics

import tfdb.semantics.OpenHOAS
import arithmetic.Num

class ForallOpenHOAS[P[_]](implicit P: Strings[P]) extends Forall[OpenHOAS[P, ?, ?], Strings] {
  def apply[E] = new Strings[OpenHOAS[P, E, ?]] {

    override def string(i: String): OpenHOAS[P, E, String] =
      OpenHOAS { _ =>
        P.string(i)
      }

    override def length(i: OpenHOAS[P, E, String]): OpenHOAS[P, E, Num] =
      OpenHOAS { env =>
        P.length(i(env))
      }
  }
}
