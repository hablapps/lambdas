package lambdas
package idb

sealed abstract class Lambda[E, T]

case class Vz[E, T]() extends Lambda[(T, E), T]

case class Vs[E, T, T1, L <: Lambda[E, T]](a: L) extends Lambda[(T1, E), T]

case class Lam[E, T1, T2, L <: Lambda[(T1, E), T2]](body: L) extends Lambda[E, T1 => T2]

case class App[E, T1, T2, Lf <: Lambda[E, T1 => T2], L1 <: Lambda[E, T1]](f: Lf, t1: L1)
    extends Lambda[E, T2]
