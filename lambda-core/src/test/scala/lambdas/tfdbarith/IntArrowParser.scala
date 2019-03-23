package lambdas
package tfdbarith

import scala.language.postfixOps

import trees._, arithparser._, tfdbparser._, ArrowParser._
import tfdb.Lambda
import arithmetic.Arithmetic

object IntArrowParser {

  def apply[P[_, _]: Lambda: Forall[?[_, _], Arithmetic]] =
    ArrowParser(IntArrowType.parser) orElse
    IntParser.forall[P, IntArrowType] close
}
