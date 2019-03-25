package lambdas
package ihoass

sealed abstract class Lambda[P[_], T]
case class Var[P[_], T](p: P[T]) extends Lambda[P, T]
case class Lam[P[_], T1, T2, L2[L1 <: Lambda[P, T1]] <: Lambda[P, T2]](f: Lambda.Nat[P, T1, T2, L2])
    extends Lambda[P, T1 => T2]
case class App[P[_], T1, T2, L1 <: Lambda[P, T1 => T2], L2 <: Lambda[P, T1]](
    f: L1,
    t1: L2
) extends Lambda[P, T2]

object Lambda {

  trait Nat[P[_], T1, T2, L2[L1 <: ihoass.Lambda[P, T1]] <: ihoass.Lambda[P, T2]] {
    def apply[L1 <: ihoass.Lambda[P, T1]](a: L1): L2[L1]
  }
}
