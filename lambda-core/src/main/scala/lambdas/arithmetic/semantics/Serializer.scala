package lambdas
package arithmetic
package semantics

import trees._, TreeSerializable.ShowTree

object Serializer extends Arithmetic[ShowTree] {

  import Constructors._

  def int(i: Int): ShowTree[Int] =
    _ => tr_int(i)

  def add(i1: ShowTree[Int])(i2: ShowTree[Int]): ShowTree[Int] =
    (i: Int) => tr_add(i1(i), i2(i))

  def * : ShowTree[(Int, Int) => Int] =
    _ => Leaf("*")

  def + : ShowTree[(Int, Int) => Int] =
    _ => Leaf("+")

  trait Constructors {
    def tr_int(i: Int): Tree =
      Node("Int", List(Leaf(i.toString)))

    def tr_add(i: Tree, j: Tree): Tree =
      Node("Add", List(i, j))
  }

  object Constructors extends Constructors
}
