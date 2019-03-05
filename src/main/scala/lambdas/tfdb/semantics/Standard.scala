package lambdas
package tfdb
package semantics

object Standard extends Lambda[Function1]{

  def int[E](i: Int): E => Int =
    _ => i

  def add[E](i1: E => Int, i2: E => Int): E => Int =
    e => i1(e) + i2(e)

  def vz[E, T]: ((T, E)) => T =
    _._1

  def vs[E, T, T1](a: E => T): ((T1, E)) => T =
    t1E => a(t1E._2)

  def lam[E, T1, T2](t: ((T1, E)) => T2): E => T1 => T2 =
    e => t1 => t((t1, e))

  def app[E, T1, T2](f: E => T1 => T2)(t1: E => T1): E => T2 =
    e => (f(e))(t1(e))
}
