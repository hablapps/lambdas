package lambdas
package initial
package hoas
package semantics

import tfhoas.{ Lambda => FLambda }
import arithmetic.Arithmetic

case class FinallyLambda[Type[_]: ArrowType, P[_]]()(
    implicit F: FLambda[Type, P],
    A: Arithmetic[P]
) {

  def apply[T](l: Lambda[Type, P, T]): P[T] = l match {
    case Var(v, _)         => v
    case Lam(f, t1, t2)    => apply_lam(f, t1, t2)
    case App(f, a, t1, t2) => F.app(apply(f))(apply(a))(t1, t2)
  }

  def apply_lam[T1, T2](f: Lambda[Type, P, T1] => Lambda[Type, P, T2], T1: Type[T1], T2: Type[T2]) =
    F.lam { pt: P[T1] =>
      apply(f(Var(pt, T1)))
    }(T1, T2)
}
