package lambdas
package arithmetic
package semantics

object ShowBArith extends Forall[ShowB, Arithmetic] {
  def apply[E] = new Arithmetic[ShowB[E, ?]] {
    def int(i: Int): ShowB[E, Int] =
      ShowArithFun.int(i)

    def add(i1: ShowB[E, Int])(i2: ShowB[E, Int]): ShowB[E, Int] =
      ShowArithFun.add(i1)(i2)

    def * : ShowB[E, (Int, Int) => Int] =
      ShowArithFun.*

    def + : ShowB[E, (Int, Int) => Int] =
      ShowArithFun.+
  }
}
