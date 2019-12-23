package lambdas
package arithmetic
package semantics

import tfdb.semantics.OpenHOAS

class ForallOpenHOAS[P[_]](implicit P: Arithmetic[P])
    extends Forall[OpenHOAS[P, ?, ?], Arithmetic] {
  def apply[E] = new Arithmetic[OpenHOAS[P, E, ?]] {

    override def int(i: Num): OpenHOAS[P, E, Num] =
      OpenHOAS { _ =>
        P.int(i)
      }

    override def abs(i: OpenHOAS[P, E, Num]): OpenHOAS[P, E, Num] =
      OpenHOAS { env =>
        P.abs(i(env))
      }

    override def add(i1: OpenHOAS[P, E, Num])(i2: OpenHOAS[P, E, Num]): OpenHOAS[P, E, Num] =
      OpenHOAS { env =>
        P.add(i1(env))(i2(env))
      }

    override def mult(i1: OpenHOAS[P, E, Num])(i2: OpenHOAS[P, E, Num]): OpenHOAS[P, E, Num] =
      OpenHOAS { env =>
        P.mult(i1(env))(i2(env))
      }

    override def max(i1: OpenHOAS[P, E, Num])(i2: OpenHOAS[P, E, Num]): OpenHOAS[P, E, Num] =
      OpenHOAS { env =>
        P.max(i1(env))(i2(env))
      }

    override def min(i1: OpenHOAS[P, E, Num])(i2: OpenHOAS[P, E, Num]): OpenHOAS[P, E, Num] =
      OpenHOAS { env =>
        P.min(i1(env))(i2(env))
      }
  }
}
