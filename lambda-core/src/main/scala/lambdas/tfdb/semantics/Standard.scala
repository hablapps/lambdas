package lambdas
package tfdb
package semantics

class Standard[Type[_]: ArrowType] extends Lambda[Type, Function1] {

  def vz[E, T: Type]: ((T, E)) => T =
    _._1

  def vs[E, T: Type, T1: Type](a: E => T): ((T1, E)) => T =
    t1E => a(t1E._2)

  def lam[E, T1: Type, T2: Type](t: ((T1, E)) => T2): E => T1 => T2 =
    e => t1 => t((t1, e))

  def app[E, T1: Type, T2: Type](f: E => T1 => T2)(t1: E => T1): E => T2 =
    e => (f(e))(t1(e))
}
