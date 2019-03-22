package lambdas
package intArrowParser

import trees._
import interpreters._
import tfdb._, tfdb.semantics._, ArrowParser._
import arithmetic._, arithmetic.semantics._

object IntArrowParser {

  def apply[P[_, _]: Lambda: ForAll[?[_, _], Arithmetic]] =
    ArrowParser[P, IntArrowType](IntArrowType.parser) orElse
    IntParser.forall[P, IntArrowType] close
}
