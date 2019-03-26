package lambdas
package ihoass

sealed abstract class Lambda[P[_], A, T]

case class Var[P[_], A](p: P[A]) extends Lambda[P, A, Var[P, A]]

case class Lam[P[_], A, B, TA, TB](f: Lambda[P, A, TA] => Lambda[P, B, TB])
    extends Lambda[P, A => B, Lam[P, A, B, TA, TB]]

case class App[P[_], A, B, TA, TB](
    f: Lambda[P, A, TA],
    t1: Lambda[P, B, TB]
) extends Lambda[P, B, App[P, A, B, TA, TB]]
