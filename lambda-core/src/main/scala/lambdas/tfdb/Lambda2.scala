package lambdas
package tfdb

import ArrowType.Implicits._, ProductType.Implicits._

abstract class Lambda2[Type[_]: ArrowType: ProductType: UnitType, P[E, T]] extends Serializable {
  def vz[E: Type, T: Type]: P[(T, E), T]
  def vs[E: Type, T: Type, T1: Type](a: P[E, T]): P[(T1, E), T]
  def lam[E: Type, T1: Type, T2: Type](t: P[(T1, E), T2]): P[E, T1 => T2]
  def app[E: Type, T1: Type, T2: Type](f: P[E, T1 => T2])(t1: P[E, T1]): P[E, T2]

  def lam2[E: Type, T1: Type, T2: Type, T3: Type](t: P[(T2, (T1, E)), T3]): P[E, T1 => T2 => T3] =
    lam(lam(t))
}

object Lambda2 {
  def apply[Type[_]: ArrowType: ProductType: UnitType, P[E, T]](implicit L: Lambda2[Type, P]) = L
}
