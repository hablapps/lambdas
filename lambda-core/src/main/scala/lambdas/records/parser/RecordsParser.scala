package lambdas
package records
package parser

import shapeless.{ ::, HList, Witness }, shapeless.labelled._
import cats.syntax.either._, cats.~>
import safecast._
import interpreters._
import trees._
import Extractors._

case class RecordParser[T[_]: RecordType: Cast, F[_]]()(
    implicit
    R: Records[F],
    IsRecord: RecordType.Match[T],
    S: Forall0[T, cats.Show]
) extends OpenInterpreter[Tree, Either[String, DynTerm[T, F]]] {

  abstract class DynFields {
    type L <: HList
    val fields: Fields[DynTerm.Aux[T, F, ?], L]
  }

  object DynFields {
    def apply[_L <: HList](_fields: Fields[DynTerm.Aux[T, F, ?], _L]): DynFields =
      new DynFields {
        type L = _L
        val fields = _fields
      }

    def build(fields: List[(String, Either[String, DynTerm[T, F]])]): Either[String, DynFields] =
      fields.foldLeft(Either.right[String, DynFields](DynFields(NilFields[DynTerm.Aux[T, F, ?]]()))) {
        case (eacc, (key, edv)) =>
          eacc flatMap { acc =>
            edv.map(
              dv =>
                DynFields(
                  HConsFields[DynTerm.Aux[T, F, ?], String, dv.A, acc.L](
                    Witness.mkWitness(key),
                    field[String](dv: DynTerm.Aux[T, F, dv.A]),
                    acc.fields
                  )
                )
            )
          }
      }
  }

  abstract class DynSelector[L <: HList] {
    type K
    type Out

    val key: Witness.Aux[K]
    val selector: Fields.Selector.Aux[F, L, K, Out]
    val typ: T[Out]
  }

  object DynSelector {

    def apply[L <: HList, _K, _Out](
        _key: Witness.Aux[_K],
        _typ: T[_Out],
        _selector: Fields.Selector.Aux[F, L, _K, _Out]
    ) =
      new DynSelector[L] {
        type K   = _K
        type Out = _Out
        val key      = _key
        val selector = _selector
        val typ      = _typ
      }

    def selector[L <: HList](fields: Fields[T, L], key: String): Option[DynSelector[L]] =
      fields match {
        case NilFields() =>
          None
        case h @ HConsFields(w, _, _) if w.value == key =>
          selectorFound(h)
        case hcons @ HConsFields(_, _, _) =>
          selectorKeepFinding(hcons, key)
      }

    def selectorFound[K, V, TL <: HList](
        hcons: HConsFields[T, K, V, TL]
    ): Option[DynSelector[FieldType[K, V] :: TL]] =
      Some(
        DynSelector[FieldType[K, V] :: TL, K, V](
          hcons.W,
          hcons.head,
          Fields.Selector.Found[F, K, V, TL]
        )
      )

    def selectorKeepFinding[K, V, TL <: HList](
        hcons: HConsFields[T, K, V, TL],
        key: String
    ): Option[DynSelector[FieldType[K, V] :: TL]] =
      for {
        dsel <- selector(hcons.tail, key)
      } yield
        DynSelector(
          dsel.key,
          dsel.typ,
          Fields.Selector.KeepFinding[F, K, V, TL, dsel.K](dsel.selector)
        )
  }

  def apply(rec: => Interpreter[Tree, Either[String, DynTerm[T, F]]]) = {
    case TreeRecord(tfields) =>
      for {
        dfields <- DynFields.build(tfields.map { kv =>
          (kv._1, rec(kv._2))
        })
      } yield
        DynTerm(
          RecordType[T].tRecord(dfields.fields(λ[DynTerm.Aux[T, F, ?] ~> T](_.typ))),
          R.record(dfields.fields(λ[DynTerm.Aux[T, F, ?] ~> F](_.term)))
        )

    case TreeSelect(tr1, k1) =>
      for {
        dr1 <- rec(tr1)
        isr <- IsRecord.unapply(dr1.typ).toEither(s"\nNot a record type: ${S().show(dr1.typ)}")
        dselect <- DynSelector
          .selector[isr.L](isr.tFields, k1)
          .toEither(s"\nNo selector $k1 in record type ${S().show(dr1.typ)}")
      } yield DynTerm(dselect.typ, R.field(isr.as(dr1.term), dselect.key)(dselect.selector))

    case t => Left(s"\nNot a record term $t")
  }
}

object RecordParser {

  import trees.tfdbparser.ArrowParser.OpenInterpreterEither

  def forall[P[_, _]: Forall[?[_, _], Records], T[_]: RecordType.Match: RecordType: Cast: Forall0[?[
    _
  ], cats.Show]] =
    new Forall[P, OpenInterpreterEither[T, ?[_]]] {
      def apply[E] = RecordParser[T, P[E, ?]]()
    }
}
