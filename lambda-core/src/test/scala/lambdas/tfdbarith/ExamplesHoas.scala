package lambdas
package tfdbarith

import tfhoas.Lambda
import arithmetic._
import IntType.Implicits._
import ArrowType.Implicits._

case class ExamplesHoas[Type[_]: ArrowType: IntType, P[_]]()(
    implicit
    L: Lambda[Type, P],
    A: Arithmetic[P]
) {

  import L._, A._

  val `λf.(f 1)` : P[(Num => Num) => Num] =
    lam { f: P[Num => Num] =>
      app(f)(int(1.bd))
    }

  val `(1+3)` : P[Num] =
    A.add(int(1.bd))(int(3.bd))

  val `λf.((f 1)+2)` : P[(Num => Num) => Num] =
    lam { f: P[Num => Num] =>
      A.add(app(f)(int(1.bd)))(int(2.bd))
    }

  val `λx0.λx1.x0`: P[Num => Num => Num] =
    lam { x0: P[Num] =>
      lam { _: P[Num] =>
        x0
      }
    }
}
