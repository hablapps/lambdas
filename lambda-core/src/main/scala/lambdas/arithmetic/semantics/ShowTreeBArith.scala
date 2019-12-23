package lambdas
package arithmetic
package semantics

import trees._, TreeSerializable.{ ShowTree, ShowTreeB }

object SerializerB extends Forall[ShowTreeB, Arithmetic] {
  def apply[E] = Arithmetic[ShowTree]
}
