package lambdas
package strings
package semantics

import arithmetic._

object ShowStrings extends Strings[Show] with Forall[ShowB, Strings] {

  override def apply[E] = this

  override def string(i: String): Show[String] =
    _ => i

  override def length(i: Show[String]): Show[Num] =
    c => s"Length(${i(c)})"
}
