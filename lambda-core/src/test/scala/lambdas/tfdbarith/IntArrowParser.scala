package lambdas
package tfdbarith

import scala.language.postfixOps

import trees._, arithparser._, tfdbparser._, ArrowParser._
import tfdb.Lambda
import arithmetic.Arithmetic

object IntArrowParser {

  def apply[P[_, _]: Lambda[IntArrowType, ?[_, _]]: Forall[?[_, _], Arithmetic]]
    : lambdas.interpreters.Interpreter[Tree, Result[IntArrowType, P]] =
    ArrowParser(IntArrowType.parser) orElse
    IntParser.forall[P, IntArrowType] close

}
