package lambdas
package tfdbarith

import tfhoas.Lambda
import arithmetic.Arithmetic

case class ExamplesHoas[Type[_]: ArrowType, P[_]]()(
    implicit
    L: Lambda[Type, P],
    A: Arithmetic[P],
    IT: Type[Int],
    AT: Type[Int => Int]
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
      lam { x1: P[Int] =>
        x0
      }
    }
}
