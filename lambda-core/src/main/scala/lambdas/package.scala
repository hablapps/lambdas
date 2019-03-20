package object lambdas {

  import cats.evidence.Is

  implicit class Lift2Op[A, A2, B, B2](p: (A Is A2, B Is B2)) {
    def lift2[T[_, _]]: T[A, B] Is T[A2, B2] =
      p._2.substitute[λ[X => T[A, B] Is T[A2, X]]](
        p._1.substitute[λ[X => T[A, B] Is T[X, B]]](Is.refl)
      )
  }

  implicit class EitherOp[A](o: Option[A]) {
    def toEither[B](none: B): Either[B, A] =
      o.fold[Either[B, A]](Left(none))(Right(_))
  }

  import cats.Semigroup

  implicit class OrElse[A: Semigroup, B](e1: Either[A, B]) {
    def orElse(e2: Either[A, B]): Either[A, B] =
      e1 match {
        case Right(r1) =>
          Right(r1)
        case Left(l1) =>
          e2 match {
            case Right(r2) =>
              Right(r2)
            case Left(l2) =>
              Left(Semigroup[A].combine(l1, l2))
          }
      }
  }

  def fix[A](f: (=> A) => A): A = {
    lazy val a: A = f(a)
    a
  }

  trait ForAll0[P[_], TC[_]] { self =>
    def apply[A](): TC[P[A]]
  }

  trait ForAll[P[_, _], TC[_[_]]] {
    def apply[E]: TC[P[E, ?]]
  }

  type ShowB[E, T] = Int => String
  type Show[T]     = Int => String
  type ShowP[T]    = String
}
