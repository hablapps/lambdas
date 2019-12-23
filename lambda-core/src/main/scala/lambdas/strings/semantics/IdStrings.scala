package lambdas
package strings
package semantics

import cats.Id
import arithmetic._

object IdStrings extends Strings[Id] {
  override def string(s: String): String = s
  override def length(s: String): Num    = s.length.bd
}
