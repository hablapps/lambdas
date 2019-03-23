package lambdas
package trees
package arithparser

import interpreters._
import safecast._
import _root_.lambdas.arithmetic._

object IntTypeParser {

  def apply[T[_]: IntType] =
    Interpreter[Tree, String, ATypeTerm[T]] {
      case TInt() => Right(IntType[T].tint)
    }(t => s"Not a type: $t")
}
