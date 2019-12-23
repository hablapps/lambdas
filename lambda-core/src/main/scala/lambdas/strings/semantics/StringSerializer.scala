package lambdas
package strings
package semantics

import trees._, TreeSerializable.{ ShowTree, ShowTreeB }

object SerializerStrings extends Strings[ShowTree] with Forall[ShowTreeB, Strings] {

  def apply[E] = this

  import Constructors._

  override def string(i: String): ShowTree[String] =
    _ => tr_string(i)

  override def length(i1: ShowTree[String]): ShowTree[String] =
    (i: Int) => tr_length(i1(i))

  trait Constructors {
    def tr_string(i: String): Tree =
      Node("String", List(Leaf(i.toString)))

    def tr_length(i: Tree): Tree =
      Node("Length", List(i))
  }

  object Constructors extends Constructors
}
