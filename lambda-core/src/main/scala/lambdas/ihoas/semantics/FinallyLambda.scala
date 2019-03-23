package lambdas
package initial
package hoas
package semantics

import tfhoas.{ Lambda => FLambda }
import arithmetic.Arithmetic

case class FinallyLambda[P[_]]()(implicit F: FLambda[P], A: Arithmetic[P]) {

  def apply[T](l: Lambda[P, T]): P[T] = l match {
    case IntL(i)   => A.int(i)
    case Add(i, j) => A.add(apply(i))(apply(j))
    case Var(v)    => v
    case Lam(f)    => F.lam(pt => apply(f(Var(pt))))
    case App(f, a) => F.app(apply(f))(apply(a))
  }
}
