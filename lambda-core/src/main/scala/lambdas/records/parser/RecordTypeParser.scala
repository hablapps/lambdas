package lambdas
package records
package parser

import lambdas.safecast._
import lambdas.interpreters._
import lambdas.trees._

import shapeless.{ HList, Witness }, shapeless.labelled._
import cats.syntax.either._
import Extractors._

case class RecordTypeParser[T[_]: RecordType]()
    extends OpenInterpreter[Tree, Either[String, ATypeTerm[T]]] {

  abstract class ATypeFields {
    type L <: HList
    val fields: Fields[T, L]
  }

  object ATypeFields {
    def apply[_L <: HList](_fields: Fields[T, _L]): ATypeFields =
      new ATypeFields {
        type L = _L
        val fields = _fields
      }

    def build(fields: List[(String, Either[String, ATypeTerm[T]])]): Either[String, ATypeFields] =
      fields.foldLeft(Either.right[String, ATypeFields](ATypeFields(NilFields[T]()))) {
        case (eacc, (key, eatt)) =>
          eacc flatMap { acc =>
            eatt.map(
              att =>
                ATypeFields(
                  HConsFields[T, String, att.A, acc.L](
                    Witness.mkWitness(key),
                    field[String](att.typ),
                    acc.fields
                  )
                )
            )
          }
      }
  }

  def apply(
      rec: => Interpreter[Tree, Either[String, ATypeTerm[T]]]
  ): Interpreter[Tree, Either[String, ATypeTerm[T]]] = {
    case TreeRecordType(tfields) =>
      for {
        dfields <- ATypeFields.build(tfields.map { kv =>
          (kv._1, rec(kv._2))
        })
      } yield ATypeTerm(RecordType[T].tRecord(dfields.fields))
    case t =>
      Left(s"Not a record type: $t")
  }
}
