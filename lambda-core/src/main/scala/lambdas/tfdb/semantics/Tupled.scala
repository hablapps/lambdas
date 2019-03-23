package lambdas
package tfdb
package semantics

class TupledInstance[P1[_, _], P2[_, _]](implicit L1: Lambda[P1], L2: Lambda[P2])
    extends Lambda[Tupled2[P1, P2]#λ] {

  def vz[E, T]: (P1[(T, E), T], P2[(T, E), T]) =
    (L1.vz, L2.vz)

  def vs[E, T, T1](a: (P1[E, T], P2[E, T])): (P1[(T1, E), T], P2[(T1, E), T]) =
    (L1.vs(a._1), L2.vs(a._2))

  def lam[E, T1, T2](t: (P1[(T1, E), T2], P2[(T1, E), T2])): (P1[E, T1 => T2], P2[E, T1 => T2]) =
    (L1.lam(t._1), L2.lam(t._2))

  def app[E, T1, T2](
      f: (P1[E, T1 => T2], P2[E, T1 => T2])
  )(t1: (P1[E, T1], P2[E, T1])): (P1[E, T2], P2[E, T2]) =
    (L1.app(f._1)(t1._1), L2.app(f._2)(t1._2))
}
