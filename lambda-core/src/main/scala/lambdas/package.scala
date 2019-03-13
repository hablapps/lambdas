package object lambdas {

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

  type ShowB[E, T] = Int => String
  type Show[T]     = Int => String
  type ShowP[T]    = String
}
