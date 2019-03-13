package lambdas
package arithmetic
package semantics

object ShowArithFun extends Arithmetic[Show] {

  def int(i: Int): Show[String] =
    _ => i.toString

  def add(i1: Show[String])(i2: Show[String]): Show[String] =
    c => s"(${i1(c)}+${i2(c)})"

  def * : Show[(Int, Int) => Int] =
    _ => "*"

  def + : Show[(Int, Int) => Int] =
    _ => "+"
}
