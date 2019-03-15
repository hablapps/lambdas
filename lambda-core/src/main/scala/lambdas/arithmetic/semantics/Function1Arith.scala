package lambdas
package arithmetic
package semantics

object Function1Arith extends ForAll[Function1, Arithmetic] {
  def apply[E] = new Arithmetic[Function1[E, ?]] {
    def int(i: Int): Function1[E, Int] = _ => ArithmeticId.int(i)

    def add(i1: Function1[E, Int])(i2: Function1[E, Int]): Function1[E, Int] =
      env => ArithmeticId.add(i1(env))(i2(env))

    def * : Function1[E, (Int, Int) => Int] = _ => ArithmeticId.*

    def + : Function1[E, (Int, Int) => Int] = _ => ArithmeticId.+
  }
}
