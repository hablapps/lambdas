package lambdas
package arithmetic
package semantics

class Tupled1Instance[P1[_], P2[_]](implicit A1: Arithmetic[P1], A2: Arithmetic[P2])
    extends Arithmetic[Tupled1[P1, P2]#λ] {

  def int(i: Int): (P1[Int], P2[Int]) =
    (A1.int(i), A2.int(i))

  def add(i1: (P1[Int], P2[Int]))(i2: (P1[Int], P2[Int])): (P1[Int], P2[Int]) =
    (A1.add(i1._1)(i2._1), A2.add(i1._2)(i2._2))

  def * : (P1[(Int, Int) => Int], P2[(Int, Int) => Int]) =
    (A1.*, A2.*)

  def + : (P1[(Int, Int) => Int], P2[(Int, Int) => Int]) =
    (A1.+, A2.+)
}
