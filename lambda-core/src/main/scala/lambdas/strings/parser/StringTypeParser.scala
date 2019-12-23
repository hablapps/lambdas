package lambdas
package strings
package parser

import trees._
import interpreters._
import safecast._
import Extractors._

object StringTypeParser {

  def apply[T[_]: StringType] =
    Interpreter[Tree, String, ATypeTerm[T]] {
      case TString() => Right(StringType[T].tString)
    }(t => s"Not a String type: $t")
}
