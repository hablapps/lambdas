package lambdas
package arithmetic
package semantics

class Tupled1Instance[P1[_], P2[_]](implicit A1: Arithmetic[P1], A2: Arithmetic[P2])
    extends Arithmetic[Tupled1[P1, P2]#λ] {

  override def int(i: Num): (P1[Num], P2[Num]) =
    (A1.int(i), A2.int(i))

  override def abs(i: (P1[Num], P2[Num])): (P1[Num], P2[Num]) =
    (A1.abs(i._1), A2.abs(i._2))

  override def add(i1: (P1[Num], P2[Num]))(i2: (P1[Num], P2[Num])): (P1[Num], P2[Num]) =
    (A1.add(i1._1)(i2._1), A2.add(i1._2)(i2._2))

  override def mult(i1: (P1[Num], P2[Num]))(i2: (P1[Num], P2[Num])): (P1[Num], P2[Num]) =
    (A1.mult(i1._1)(i2._1), A2.mult(i1._2)(i2._2))

  override def max(i1: (P1[Num], P2[Num]))(i2: (P1[Num], P2[Num])): (P1[Num], P2[Num]) =
    (A1.max(i1._1)(i2._1), A2.max(i1._2)(i2._2))

  override def min(i1: (P1[Num], P2[Num]))(i2: (P1[Num], P2[Num])): (P1[Num], P2[Num]) =
    (A1.min(i1._1)(i2._1), A2.min(i1._2)(i2._2))
}
