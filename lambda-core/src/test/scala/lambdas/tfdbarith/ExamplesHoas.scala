package lambdas
package tfdbarith

import tfhoas.Lambda
import arithmetic.{ Arithmetic, IntType }, IntType.Implicits._, ArrowType.Implicits._

case class ExamplesHoas[Type[_]: ArrowType: IntType, P[_]]()(
    implicit
    L: Lambda[Type, P],
    A: Arithmetic[P]
) {

  import L._, A._

  val `λf.(f 1)` : P[(Int => Int) => Int] =
    lam { f: P[Int => Int] =>
      app(f)(int(1))
    }

  val `(1+3)` : P[Int] =
    add(int(1))(int(3))

  val `λf.((f 1)+2)` : P[(Int => Int) => Int] =
    lam { f: P[Int => Int] =>
      add(app(f)(int(1)))(int(2))
    }

  val `λx0.λx1.x0`: P[Int => Int => Int] =
    lam { x0: P[Int] =>
      lam { _: P[Int] =>
        x0
      }
    }
}
