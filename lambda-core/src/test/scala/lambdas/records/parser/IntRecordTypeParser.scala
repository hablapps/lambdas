package lambdas
package records
package parser

import scala.language.postfixOps
import cats.instances.string._
import safecast.ATypeTerm
import interpreters._
import trees._, arithparser._, tfdbparser._

object IntRecordTypeParser {
  val apply: Interpreter[Tree, Either[String, ATypeTerm[IntRecordType]]] =
    ArrowTypeParser[IntRecordType] orElse
    new RecordTypeParser[IntRecordType] orElse
    IntTypeParser[IntRecordType] close
}
