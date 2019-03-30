package lambdas
package arithmetic

import cats.evidence._
import safecast._

trait IntType[T[_]] {
  def tint: T[Int]
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

    implicit val _Int: T[Int] =
      Implicits._Int(_IntType)
  }

  object Implicits {
    implicit def _Int[T[_]](implicit T: IntType[T]): T[Int] =
      T.tint
  }
}

trait IntTypeDeserialization {

  case class Case[A](is: A Is Int)

  object Case {
    implicit val _IntTypeCase = new IntType[λ[T => Option[Case[T]]]] {
      def tint: Option[Case[Int]] =
        Option(Case(Is.refl[Int]))
    }
  }

  type Match[T[_]] = safecast.Match[T, Case]

  implicit def IntTypeDeserialization[T[_]: IntType](implicit IsInt: Match[T]) =
    new IntType[Cast.As[T, ?]] {
      def tint = new Cast.As[T, Int] {
        def apply[T2](t2: T[T2]): Option[Int Is T2] =
          IsInt.unapply(t2) map (_.is.flip)
      }
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
