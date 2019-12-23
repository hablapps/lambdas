package lambdas
package arithmetic
package semantics

import trees._, TreeSerializable.ShowTree

object ArithmeticSerializer extends Arithmetic[ShowTree] {

  import Constructors._

  override def int(i: Num): ShowTree[Num] =
    _ => tr_int(i)

  override def abs(i1: ShowTree[Num]): ShowTree[Num] =
    (i: Int) => tr_abs(i1(i))

  override def add(i1: ShowTree[Num])(i2: ShowTree[Num]): ShowTree[Num] =
    (i: Int) => tr_add(i1(i), i2(i))

  override def mult(i1: ShowTree[Num])(i2: ShowTree[Num]): ShowTree[Num] =
    (i: Int) => tr_mult(i1(i), i2(i))

  override def max(i1: ShowTree[Num])(i2: ShowTree[Num]): ShowTree[Num] =
    (i: Int) => tr_max(i1(i), i2(i))

  override def min(i1: ShowTree[Num])(i2: ShowTree[Num]): ShowTree[Num] =
    (i: Int) => tr_min(i1(i), i2(i))

  trait Constructors {
    def tr_int(i: Num): Tree =
      Node("Int", List(Leaf(i.toString)))

    def tr_abs(i: Tree): Tree =
      Node("Abs", List(i))

    def tr_add(i: Tree, j: Tree): Tree =
      Node("Add", List(i, j))

    def tr_mult(i: Tree, j: Tree): Tree =
      Node("Mult", List(i, j))

    def tr_max(i: Tree, j: Tree): Tree =
      Node("Max", List(i, j))

    def tr_min(i: Tree, j: Tree): Tree =
      Node("Min", List(i, j))
  }

  object Constructors extends Constructors
}
