package lambdas
package intArrowParser

import trees._, arithparser._, tfdbparser._, ArrowParser._
import tfdb.Lambda
import arithmetic.Arithmetic

object IntArrowParser {

  def apply[P[_, _]: Lambda: ForAll[?[_, _], Arithmetic]] =
    ArrowParser(IntArrowType.parser) orElse
    IntParser.forall[P, IntArrowType] close
}
