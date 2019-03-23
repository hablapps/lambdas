package lambdas
package arithmetic
package semantics

import cats.Id

object ArithmeticId extends Arithmetic[Id] {

  def int(i: Int): Int = i

  def add(i1: Int)(i2: Int): Int = i1 + i2

  def * : (Int, Int) => Int = _ * _

  def + : (Int, Int) => Int = _ + _
}
