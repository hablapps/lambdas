package lambdas
package idb

sealed abstract class Lambda[E, A, T <: Lambda[E, A, T]]

case class Vz[E, A]() extends Lambda[(A, E), A, Vz[E, A]]

case class Vs[E, A, A1, L <: Lambda[E, A, L]](a: Lambda[E, A, L])
    extends Lambda[(A1, E), A, Vs[E, A, A1, L]]

case class Lam[E, A1, A2, L <: Lambda[(A1, E), A2, L]](body: Lambda[(A1, E), A2, L])
    extends Lambda[E, A1 => A2, Lam[E, A1, A2, L]]

case class App[E, A1, A2, Lf <: Lambda[E, A1 => A2, Lf], L1 <: Lambda[E, A1, L1]](
    f: Lambda[E, A1 => A2, Lf],
    t1: Lambda[E, A1, L1]
) extends Lambda[E, A2, App[E, A1, A2, Lf, L1]]
