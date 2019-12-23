package lambdas
package records

import lambdas.records.semantics.Record
import shapeless.labelled.FieldType
import shapeless.{ HList, Witness }

trait Records[P[_]] {

  def record[L <: HList](fields: Fields[P, L]): P[Record[L]]

  def field[L <: HList, K](record: P[Record[L]], key: Witness.Aux[K])(
      implicit
      S: Fields.Selector[P, L, K]
  ): P[S.Out]
}

object Records extends LPI {

  def apply[P[_]](implicit R: Records[P]) = R

  object ->> {
    def unapply[K, V](f: FieldType[K, V])(implicit k: Witness.Aux[K]) = Option((k.value, f: V))
  }

  implicit val recordsId        = semantics.StdRecords
  implicit val ShowRecords      = semantics.ShowRecords
  implicit val Serializer       = semantics.Serializer
  implicit val Function1Records = semantics.Function1Records
  implicit def SerializerB      = semantics.SerializerB
  implicit def IsoRecords[P[_]](
      implicit
      Iso: IsoGen[P, HList, Record, semantics.GenRecord.Aux[P]#λ]
  ) =
    new semantics.GenRecord.IsoRecords[P]
}

trait LPI {

  implicit def RecordsForall[E, P[_, _]](implicit FA: Forall[P, Records]): Records[P[E, ?]] =
    FA[E]

  implicit def ForallRecords[P[_]](implicit P: Records[P]) =
    new semantics.ForallOpenHOAS[P]
}
