package lambdas
package arithmetic
package semantics

class TupledInstance[P1[_, _], P2[_, _]](
    implicit
    FA1: ForAll[P1, Arithmetic],
    FA2: ForAll[P2, Arithmetic]
) extends ForAll[Tupled2[P1, P2]#λ, Arithmetic] {

  def apply[E] = new Arithmetic[Tupled2[P1, P2]#λ[E, ?]] {
    val T1 = new Tupled1Instance[P1[E, ?], P2[E, ?]]

    def int(i: Int): (P1[E, Int], P2[E, Int]) =
      T1.int(i)

    def add(i1: (P1[E, Int], P2[E, Int]))(i2: (P1[E, Int], P2[E, Int])): (P1[E, Int], P2[E, Int]) =
      T1.add(i1)(i2)

    def * : (P1[E, (Int, Int) => Int], P2[E, (Int, Int) => Int]) =
      T1.*

    def + : (P1[E, (Int, Int) => Int], P2[E, (Int, Int) => Int]) =
      T1.+
  }
}
