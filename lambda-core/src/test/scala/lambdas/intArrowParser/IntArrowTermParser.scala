package lambdas
package intArrowParser

import trees._
import interpreters._
import tfdb._, tfdb.semantics._, ArrowParser._
import arithmetic._, arithmetic.semantics._

object IntArrowTermParser {

  def apply[P[_, _]: Lambda: ForAll[?[_, _], Arithmetic]] =
    ArrowParser[P, IntArrowTypeTerm](IntArrowTypeTermParser.apply) orElse
    IntParser.forall[P, IntArrowTypeTerm] close
}
