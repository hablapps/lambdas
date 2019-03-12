package lambdas
package arithmetic
package semantics

object ShowArithFun extends Arithmetic[Show] {

  def * : Show[(Int, Int) => Int] =
    _ => "*"

  def + : Show[(Int, Int) => Int] =
    _ => "+"
}
