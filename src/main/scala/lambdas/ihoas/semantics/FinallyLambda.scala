package lambdas
package initial
package hoas
package semantics

import taglessfinal.hoas.{Lambda => FLambda}

case class FinallyLambda[P[_]]()(implicit F: FLambda[P]){

  def apply[T](l: Lambda[P, T]): P[T] = l match {
    case IntL(i) => F.int(i)
    case Add(i, j) => F.add(apply(i))(apply(j))
    case Var(v) => v
    case Lam(f) => F.lam(pt => apply(f(Var(pt))))
    case App(f, a) => F.app(apply(f))(apply(a))
  }
}
