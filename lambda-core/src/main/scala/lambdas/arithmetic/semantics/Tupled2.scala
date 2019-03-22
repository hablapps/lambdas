package lambdas
package arithmetic
package semantics

class TupledInstance[P1[_, _], P2[_, _]](
    implicit
    FA1: ForAll[P1, Arithmetic],
    FA2: ForAll[P2, Arithmetic]
) extends ForAll[Tupled2[P1, P2]#λ, Arithmetic] {

  def apply[E] = new Arithmetic[Tupled2[P1, P2]#λ[E, ?]] {

    def int(i: Int): (P1[E, Int], P2[E, Int]) =
      (FA1[E].int(i), FA2[E].int(i))

    def add(i1: (P1[E, Int], P2[E, Int]))(i2: (P1[E, Int], P2[E, Int])): (P1[E, Int], P2[E, Int]) =
      (FA1[E].add(i1._1)(i2._1), FA2[E].add(i1._2)(i2._2))

    def * : (P1[E, (Int, Int) => Int], P2[E, (Int, Int) => Int]) =
      (FA1[E].*, FA2[E].*)

    def + : (P1[E, (Int, Int) => Int], P2[E, (Int, Int) => Int]) =
      (FA1[E].+, FA2[E].+)
  }
}
