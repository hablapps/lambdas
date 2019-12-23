package lambdas
package arithmetic
package semantics

object Function1Arith extends Forall[Function1, Arithmetic] {
  def apply[E] = new Arithmetic[Function1[E, ?]] {
    override def int(i: Num): Function1[E, Num] = _ => ArithmeticId.int(i)

    override def abs(i: E => Num): E => Num =
      env => ArithmeticId.abs(i(env))

    override def add(i1: Function1[E, Num])(i2: Function1[E, Num]): Function1[E, Num] =
      env => ArithmeticId.add(i1(env))(i2(env))

    override def mult(i1: Function1[E, Num])(i2: Function1[E, Num]): Function1[E, Num] =
      env => ArithmeticId.mult(i1(env))(i2(env))

    override def max(i1: E => Num)(i2: E => Num): E => Num =
      env => ArithmeticId.max(i1(env))(i2(env))

    override def min(i1: E => Num)(i2: E => Num): E => Num =
      env => ArithmeticId.min(i1(env))(i2(env))
  }
}
