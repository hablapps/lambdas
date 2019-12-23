package lambdas
package records
package parser

import scala.language.postfixOps

import lambdas.interpreters._
import trees._, arithparser._, tfdbparser._, ArrowParser._
import tfdb.Lambda
import arithmetic.Integers

object IntRecordParser {

  def apply[
      P[_, _]: Lambda[IntRecordType, ?[_, _]]: Forall[?[_, _], Records]: Forall[?[_, _], Integers]
  ]: Interpreter[Tree, Result[IntRecordType, P]] =
    ArrowParser(IntRecordTypeParser.apply) orElse
    IntegerParser.forall[P, IntRecordType] orElse
    RecordParser.forall[P, IntRecordType] close

  // def apply[P[_]: Records: Integers]: Interpreter[Tree, Either[String, DynTerm[IntRecordType, P]]] =
  //   ArrowParser(IntRecordTypeParser.apply) orElse
  //   IntegerParser[IntRecordType, P] orElse
  //   RecordParser[IntRecordType, P] close

}
