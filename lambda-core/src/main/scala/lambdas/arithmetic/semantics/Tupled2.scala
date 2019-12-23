package lambdas
package arithmetic
package semantics

class TupledInstance[P1[_, _], P2[_, _]](
    implicit
    FA1: Forall[P1, Arithmetic],
    FA2: Forall[P2, Arithmetic]
) extends Forall[Tupled2[P1, P2]#λ, Arithmetic] {

  def apply[E] = new Arithmetic[Tupled2[P1, P2]#λ[E, ?]] {
    val T1 = new Tupled1Instance[P1[E, ?], P2[E, ?]]

    override def int(i: Num): (P1[E, Num], P2[E, Num]) =
      T1.int(i)

    override def abs(i: (P1[E, Num], P2[E, Num])): (P1[E, Num], P2[E, Num]) =
      T1.abs(i)

    override def add(
        i1: (P1[E, Num], P2[E, Num])
    )(i2: (P1[E, Num], P2[E, Num])): (P1[E, Num], P2[E, Num]) =
      T1.add(i1)(i2)

    override def mult(
        i1: (P1[E, Num], P2[E, Num])
    )(i2: (P1[E, Num], P2[E, Num])): (P1[E, Num], P2[E, Num]) =
      T1.mult(i1)(i2)

    override def max(
        i1: (P1[E, Num], P2[E, Num])
    )(i2: (P1[E, Num], P2[E, Num])): (P1[E, Num], P2[E, Num]) =
      T1.max(i1)(i2)

    override def min(
        i1: (P1[E, Num], P2[E, Num])
    )(i2: (P1[E, Num], P2[E, Num])): (P1[E, Num], P2[E, Num]) =
      T1.min(i1)(i2)
  }
}
