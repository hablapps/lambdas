package lambdas
package arithmetic
package semantics

import cats.Id

object ArithmeticId extends Arithmetic[Id] {

  def * : (Int, Int) => Int = _ * _

  def + : (Int, Int) => Int = _ + _
}
