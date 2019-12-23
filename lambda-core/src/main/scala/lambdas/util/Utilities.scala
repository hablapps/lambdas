package util

trait Utilities {

  case class Iso[A, B](get: A => B, put: B => A)

  case class Setter[A, B](put: B => A)

  trait IsoGen[Repr[_], C, A[_ <: C], B[_ <: C]] {
    def apply[T <: C]: Iso[Repr[A[T]], B[T]]
  }

  trait IsoGen2[Repr[_], A[_, _], B[_, _]] {
    def apply[T1, T2]: Iso[Repr[A[T1, T2]], B[T1, T2]]
  }

  trait SetterGen[Repr[_], A, B] {
    def apply(): Setter[Repr[A], B]
  }

  object SetterGen {
    import cats.Id

    implicit def IdSetterGen[A]: SetterGen[Id, A, A] =
      new SetterGen[Id, A, A] {
        def apply(): Setter[A, A] =
          Setter(identity[A])
      }
  }

  import cats.evidence.Is

  implicit class LiftBounded[T, A <: T, B <: T](is: A Is B) {
    def liftB[F[_ <: T]]: F[A] Is F[B] = is.asInstanceOf[F[A] Is F[B]]
  }

  implicit class Lift2OpBounded[T, A, A2, B <: T, B2 <: T](p: (A Is A2, B Is B2)) {
    def lift2B[F[_, _ <: T]]: F[A, B] Is F[A2, B2] =
      Is.refl[Int].asInstanceOf[F[A, B] Is F[A2, B2]]
  }

  implicit class Lift2Op[A, A2, B, B2](p: (A Is A2, B Is B2)) {
    def lift2[T[_, _]]: T[A, B] Is T[A2, B2] =
      p._2.substitute[λ[X => T[A, B] Is T[A2, X]]](
        p._1.substitute[λ[X => T[A, B] Is T[X, B]]](Is.refl)
      )
  }

  implicit class Lift3Op[A, A2, B, B2, C, C2](p: (A Is A2, B Is B2, C Is C2)) {
    def lift3[T[_, _, _]]: T[A, B, C] Is T[A2, B2, C2] =
      p._3.substitute[λ[X => T[A, B, C] Is T[A2, B2, X]]](
        p._2.substitute[λ[X => T[A, B, C] Is T[A2, X, C]]](
          p._1.substitute[λ[X => T[A, B, C] Is T[X, B, C]]](Is.refl)
        )
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

  trait Forall0[P[_], TC[_]] { self =>
    def apply[A](): TC[P[A]]
  }

  object Forall0 {
    def apply[P[_], TC[_]](implicit FA: Forall0[P, TC]) = FA
  }

  trait Forall[P[_, _], TC[_[_]]] {
    def apply[E]: TC[P[E, ?]]
  }

  trait Forall2[P[_, _], Q[_, _], TC[_[_], _[_]]] {
    def apply[E]: TC[P[E, ?], Q[E, ?]]
  }

  type ShowB[E, T] = Int => String
  type Show[T]     = Int => String
  type ShowP[T]    = String

  trait ShowE[P[_], T] {
    def show(t: T): P[String]
  }

  trait Tupled1[P1[_], P2[_]] {
    type λ[A] = (P1[A], P2[A])
  }

  trait Tupled2[P1[_, _], P2[_, _]] {
    type λ[E, A] = (P1[E, A], P2[E, A])
  }

  implicit object SeqFunctor extends cats.Functor[Seq] {
    def map[A, B](s: Seq[A])(f: A => B): Seq[B] = s.map(f)
  }

  // TODO: Replace with a combination of Leibiniz and Forall
  trait Composed[P[_], F[_], G[_]] {
    def apply[A](p: P[A]): F[G[A]]
  }
  object Composed {
    implicit def _composed[F[_], G[_]] =
      new Composed[Lambda[t => F[G[t]]], F, G] {
        override def apply[A](p: F[G[A]]): F[G[A]] = p
      }
  }
}
