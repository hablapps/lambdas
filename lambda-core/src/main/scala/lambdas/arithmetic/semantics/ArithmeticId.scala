package lambdas
package arithmetic
package semantics

import cats.Id

object ArithmeticId extends Arithmetic[Id] {

  override def int(i: Num): Num = i

  override def abs(i: Num): Num = if (i < 0) -i else i

  override def add(i1: Num)(i2: Num) = i1 + i2

  override def mult(i1: Num)(i2: Num) = i1 * i2

  override def max(i1: Num)(i2: Num) = if (i1 > i2) i1 else i2

  override def min(i1: Num)(i2: Num): Num =
    if (i1 > i2) i2 else i1
}
