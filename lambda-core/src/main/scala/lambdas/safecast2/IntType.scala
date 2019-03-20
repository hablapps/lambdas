package lambdas
package safecast2

import cats.evidence._

trait IntType[T[_]] {
  def tint: T[Int]
}

object IntType {

  def apply[T[_]](implicit T: IntType[T]) = T

  case class Case[A](is: A Is Int)

  object Case {
    implicit val Case_IntType = new IntType[λ[T => Option[Case[T]]]] {
      def tint: Option[Case[Int]] =
        Option(Case(Is.refl[Int]))
    }
  }

  implicit val _ShowP = new IntType[ShowP] {
    def tint: String = "TInt"
  }

  implicit def IntTypeCast[T[_]: IntType](implicit IsInt: Match[T, Case]) =
    new IntType[Cast.As[T, ?]] {
      def tint = new Cast.As[T, Int] {
        def apply[T2](t2: T[T2]): Option[Int Is T2] =
          IsInt.unapply(t2) map (_.is.flip)
      }
    }
}
