package lambdas
package intArrowParser

import cats.instances.string._

import trees._, syntax._
import safecast._
import interpreters._
import arithmetic.IntType
import tfdb.ArrowType

object IntArrowTypeTermParser {

  val apply: Interpreter[Tree, Either[String, ATypeTerm[IntArrowTypeTerm]]] =
    ParseLambdaType orElse
    ParseIntType close

  lazy val ParseLambdaType = OpenInterpreter[Tree, String, ATypeTerm[IntArrowTypeTerm]] { rec =>
    {
      case TArr(t1, t2) =>
        for {
          t1t <- rec(t1)
          t2t <- rec(t2)
        } yield ATypeTerm(ArrowType[IntArrowTypeTerm].tarrow(t1t.typ, t2t.typ))
    }
  }(t => s"Not a type: $t")

  lazy val ParseIntType = Interpreter[Tree, String, ATypeTerm[IntArrowTypeTerm]] {
    case TInt() => Right(IntType[IntArrowTypeTerm].tint)
  }(t => s"Not a type: $t")
}
