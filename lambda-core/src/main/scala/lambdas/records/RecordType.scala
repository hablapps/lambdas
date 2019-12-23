package lambdas
package records

import cats.~>
import semantics.Record
import shapeless.{ ::, HList, HNil, Witness }, shapeless.labelled._

trait RecordType[T[_]] extends Serializable {
  def tRecord[L <: HList](a: Fields[T, L]): T[Record[L]]
}

object RecordType
    extends RecordTypeDeserialization
    with RecordTypeImplicits
    with RecordTypeSerialization
    with RecordTypeLPI {

  def apply[T[_]](implicit T: RecordType[T]) = T
}

trait RecordTypeImplicits {

  trait Implicits[T[_]] {
    val _RecordType: RecordType[T]

    implicit def _NilFields: Fields[T, HNil] = NilFields()

    implicit def _HConsFields[K, V, TL <: HList](
        implicit
        TL: Fields[T, TL],
        V: T[V],
        W: Witness.Aux[K]
    ): Fields[T, FieldType[K, V] :: TL] =
      HConsFields(W, field[K](V), TL)

    implicit def _RecordType_T[L <: HList](implicit F: Fields[T, L]): T[Record[L]] =
      Implicits._RecordType_T(F, _RecordType)
  }

  object Implicits {
    implicit def _RecordType_T[T[_], L <: HList](
        implicit F: Fields[T, L],
        T: RecordType[T]
    ): T[Record[L]] =
      T.tRecord(F)
  }
}

trait RecordTypeDeserialization extends RecordTypeDeserializationLPI {
  import cats.evidence._
  import safecast._

  abstract class Case[T[_], R] {
    type L <: HList

    val tFields: Fields[T, L]
    val is: R Is Record[L]

    def as[F[_]](r: F[R]): F[Record[L]] =
      is.substitute[F](r)

    def asT(r: T[R]): T[Record[L]] =
      is.substitute[T](r)
  }

  type Match[T[_]] = lambdas.safecast.Match[T, Case[T, ?]]

  implicit def Case_RecordType[T[_]: RecordType]: RecordType[λ[A => (T[A], Option[Case[T, A]])]] = {
    type Alias[A] = (T[A], Option[Case[T, A]])
    new RecordType[Alias] {
      def tRecord[_L <: HList](otA: Fields[Alias, _L]) = {
        val fieldsT = otA(λ[Alias ~> T](_._1))
        (RecordType[T].tRecord(fieldsT), Option(new Case[T, Record[_L]] {
          type L = _L

          val tFields = fieldsT
          val is      = Is.refl[Record[L]]
        }))
      }
    }
  }

  class IsHList[T[_]] {
    type Alias[α] = Cast.As[T, α]

    def eq[L1 <: HList, L2 <: HList](f1: Fields[Alias, L1], f2: Fields[T, L2]): Option[L1 Is L2] =
      (f1, f2) match {
        case (NilFields(), NilFields()) => Some(Is.refl)
        case (f1 @ HConsFields(w1, _, _), f2 @ HConsFields(w2, _, _))
            if w1.value.toString == w2.value.toString =>
          hcons(f1, f2)
        case _ => None
      }

    import shapeless.::, shapeless.labelled.FieldType

    def hcons[K, V1, T1 <: HList, V2, T2 <: HList](
        f1: HConsFields[Alias, K, V1, T1],
        f2: HConsFields[T, K, V2, T2]
    ): Option[(FieldType[K, V1] :: T1) Is (FieldType[K, V2] :: T2)] = {
      type Ali[α, β <: HList] = FieldType[K, α] :: β
      for {
        headIs <- f1.head(f2.head): Option[V1 Is V2]
        tailIs <- eq(f1.tail, f2.tail): Option[T1 Is T2]
      } yield (headIs, tailIs).lift2B[Ali]
    }
  }

  implicit def RecordTypeCast[T[_]: RecordType](
      implicit
      MatchRecord: Match[T]
  ) =
    new RecordType[Cast.As[T, ?]] {
      def tRecord[L <: HList](fields: Fields[Cast.As[T, ?], L]) =
        new Cast.As[T, Record[L]] {
          def apply[C2](t2: T[C2]): Option[Record[L] Is C2] =
            for {
              result  <- MatchRecord.unapply(t2)
              eqHList <- new IsHList[T].eq(fields, result.tFields)
            } yield eqHList.liftB[Record].andThen(result.is.flip)
        }
    }
}

trait RecordTypeDeserializationLPI {

  implicit def _NoneRecordTypeCase[T[_]: RecordType, Ca[T[_], _]]
    : RecordType[λ[A => (T[A], Option[Ca[T, A]])]] = {
    type Alias[A] = (T[A], Option[Ca[T, A]])

    new RecordType[Alias] {
      def tRecord[L <: HList](v1: Fields[Alias, L]): Alias[Record[L]] = {
        val fields = v1(λ[Alias ~> T](_._1))
        (RecordType[T].tRecord(fields), None)
      }
    }
  }

  implicit def RecordTypeCaseNoneGen1[Ca[_]]: RecordType[λ[A => Option[Ca[A]]]] = {
    type Alias[A] = Option[Ca[A]]

    new RecordType[Alias] {
      def tRecord[L <: HList](v1: Fields[Alias, L]): Alias[Record[L]] =
        None
    }
  }
}

trait RecordTypeSerialization {
  import trees._, TreeSerializable.ShowTree

  trait Constructors {

    def tr_tRecord(fields: List[(String, Tree)]): Tree =
      Node("RecordType", fields.map {
        case (key, typ) =>
          Node("FieldType", List(Leaf(key), typ))
      })
  }

  object Constructors extends Constructors

  implicit def _ShowTree = new RecordType[ShowTree] {

    def toTree[L <: HList](i: Int): Fields[ShowTree, L] => List[(String, Tree)] = {
      case NilFields() => List()
      case HConsFields(key, value, tail) =>
        (key.value.toString, value(i)) :: toTree(i)(tail)
    }

    def tRecord[L <: HList](fields: Fields[ShowTree, L]): Int => Tree =
      i =>
        Node("RecordType", toTree(i)(fields).map {
          case (key, typ) =>
            Node("FieldType", List(Leaf(key), typ))
        })
  }
}

trait RecordTypeLPI {

  implicit val _ShowP = new RecordType[ShowP] {
    def tRecord[L <: HList](tV1: Fields[ShowP, L]): String =
      toString(tV1).mkString("{", ", ", "}")

    def toString[L <: HList](f: Fields[ShowP, L]): List[String] = f match {
      case NilFields() => List()
      case HConsFields(key, value, tail) =>
        s"${key.value}: $value" :: toString(tail)
    }
  }
}
