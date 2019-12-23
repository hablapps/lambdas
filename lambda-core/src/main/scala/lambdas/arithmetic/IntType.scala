package lambdas
package arithmetic

import cats.evidence._
import safecast._

trait IntType[T[_]] extends Serializable {
  def tint: T[Num]
}

object IntType
    extends IntTypeImplicits
    with IntTypeSerialization
    with IntTypeDeserialization
    with IntTypeLPI {

  def apply[T[_]](implicit T: IntType[T]) = T
}

trait IntTypeImplicits {

  trait Implicits[T[_]] {
    val _IntType: IntType[T]

    implicit val _Int: T[Num] =
      Implicits._Int(_IntType)
  }

  object Implicits {
    implicit def _Int[T[_]](implicit T: IntType[T]): T[Num] =
      T.tint
  }
}

trait IntTypeDeserialization extends IntTypeDeserializationLPI {

  case class Case[A](is: A Is Num)

  implicit val _IntTypeCase = new IntType[λ[T => Option[Case[T]]]] {
    def tint: Option[Case[Num]] =
      Option(Case(Is.refl[Num]))
  }

  type Match[T[_]] = safecast.Match[T, Case]

  implicit def _CastAsIntType[T[_]: IntType](implicit IsInt: Match[T]) =
    new IntType[Cast.As[T, ?]] {
      def tint = new Cast.As[T, Num] {
        def apply[T2](t2: T[T2]): Option[Num Is T2] =
          IsInt.unapply(t2) map (_.is.flip)
      }
    }
}

trait IntTypeDeserializationLPI {

  implicit def _NoneIntTypeCase[T[_]: IntType, Ca[T[_], _]] =
    new IntType[λ[A => (T[A], Option[Ca[T, A]])]] {
      def tint: (T[Num], Option[Ca[T, Num]]) =
        (IntType[T].tint, None)
    }

  implicit def IntTypeCaseNoneGen1[Ca[_]] =
    new IntType[λ[A => Option[Ca[A]]]] {
      def tint: Option[Ca[Num]] =
        None
    }
}

trait IntTypeSerialization {
  import trees._, TreeSerializable.ShowTree

  trait Constructors {
    def tr_tInt: Tree =
      Leaf("TInt")
  }

  object Constructors extends Constructors

  implicit def _ShowTree = new IntType[ShowTree] {
    def tint: Int => Tree =
      _ => Constructors.tr_tInt
  }
}

trait IntTypeLPI {

  implicit val _ShowP = new IntType[ShowP] {
    def tint: String = "TInt"
  }
}

trait IntTypeSyntax {
  def tint[T[_]](implicit T: IntType[T]): T[Num] =
    T.tint
}
