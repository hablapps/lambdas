package lambdas
package arithmetic
package semantics

object ShowArithFun extends Arithmetic[Show] with Forall[ShowB, Arithmetic] {

  override def apply[E] = this

  override def int(i: Num): Show[String] =
    _ => i.toString

  override def abs(i: Show[Num]): Show[Num] =
    c => s"Abs(${i(c)})"

  override def add(i1: Show[Num])(i2: Show[Num]): Show[Num] =
    c => s"(${i1(c)}+${i2(c)})"

  override def mult(i1: Show[Num])(i2: Show[Num]): Show[Num] =
    c => s"(${i1(c)}*${i2(c)})"

  override def max(i1: Show[Num])(i2: Show[Num]): Show[Num] =
    c => s"Max(${i1(c)},${i2(c)})"

  override def min(i1: Show[Num])(i2: Show[Num]): Show[Num] =
    c => s"Min(${i1(c)},${i2(c)})"

}
