package lambdas

import cats.evidence._
import safecast._

trait UnitType[T[_]] extends Serializable {
  def tUnit: T[Unit]
}

object UnitType
    extends UnitTypeImplicits
    with UnitTypeSerialization
    with UnitTypeDeserialization
    with UnitTypeLPI {

  def apply[T[_]](implicit T: UnitType[T]) = T
}

trait UnitTypeImplicits {

  trait Implicits[T[_]] {
    val _UnitType: UnitType[T]

    implicit val _Unit: T[Unit] =
      Implicits._Unit(_UnitType)
  }

  object Implicits {
    implicit def _Unit[T[_]](implicit T: UnitType[T]): T[Unit] =
      T.tUnit
  }
}

trait UnitTypeDeserialization extends UnitTypeDeserializationLPI {

  case class Case[A](is: A Is Unit)

  implicit val _UnitTypeCase = new UnitType[λ[T => Option[Case[T]]]] {
    def tUnit: Option[Case[Unit]] =
      Option(Case(Is.refl[Unit]))
  }

  type Match[T[_]] = safecast.Match[T, Case]

  implicit def _CastAsUnitType[T[_]: UnitType](implicit IsInt: Match[T]) =
    new UnitType[Cast.As[T, ?]] {
      def tUnit = new Cast.As[T, Unit] {
        def apply[T2](t2: T[T2]): Option[Unit Is T2] =
          IsInt.unapply(t2) map (_.is.flip)
      }
    }
}

trait UnitTypeDeserializationLPI {

  implicit def _NoneUnitTypeCase[T[_]: UnitType, Ca[T[_], _]] =
    new UnitType[λ[A => (T[A], Option[Ca[T, A]])]] {
      def tUnit: (T[Unit], Option[Ca[T, Unit]]) =
        (UnitType[T].tUnit, None)
    }

  implicit def UnitTypeCaseNoneGen1[Ca[_]] =
    new UnitType[λ[A => Option[Ca[A]]]] {
      def tUnit: Option[Ca[Unit]] =
        None
    }
}

trait UnitTypeSerialization {
  import trees._, TreeSerializable.ShowTree

  trait Constructors {
    def tr_tUnit: Tree =
      Leaf("TUnit")
  }

  object Constructors extends Constructors

  implicit def _ShowTree = new UnitType[ShowTree] {
    def tUnit: Int => Tree =
      _ => Constructors.tr_tUnit
  }
}

trait UnitTypeLPI {

  implicit val _ShowP = new UnitType[ShowP] {
    def tUnit: String = "TUnit"
  }
}

trait UnitTypeSyntax {
  def tUnit[T[_]](implicit T: UnitType[T]): T[Unit] =
    T.tUnit
}
