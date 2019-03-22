package lambdas
package arithmetic
package semantics

import interpreters._
import safecast._
import trees._, syntax._

object IntTypeParser {

  def apply[T[_]: IntType] =
    Interpreter[Tree, String, ATypeTerm[T]] {
      case TInt() => Right(IntType[T].tint)
    }(t => s"Not a type: $t")
}
