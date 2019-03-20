package lambdas
package safecast2

import cats.evidence._

trait ArrowType[T[_]] {
  def tarrow[T1, T2](t1: T[T1], t2: T[T2]): T[T1 => T2]
}

object ArrowType {

  def apply[T[_]](implicit A: ArrowType[T]) = A

  abstract class Case[T[_], A] {
    type T1
    type T2
    val ta: T[A]
    val is: (T[T1], T[T2], A Is (T1 => T2))
  }

  object Case {
    implicit def Case_ArrowType[T[_]: ArrowType] = new ArrowType[λ[A => Option[Case[T, A]]]] {
      def tarrow[_T1, _T2](ot1: Option[Case[T, _T1]], ot2: Option[Case[T, _T2]]) =
        for {
          t1 <- ot1
          t2 <- ot2
        } yield
          new Case[T, _T1 => _T2] {
            type T1 = _T1
            type T2 = _T2
            val ta = ArrowType[T].tarrow(t1.ta, t2.ta)
            val is = (t1.ta, t2.ta, Is.refl[T1 => T2])
          }
    }
  }

  trait Match[T[_]] {
    def unapply[A](t: T[A]): Option[Case[T, A]]
  }

  implicit def ArrowTypeCast[T[_]: ArrowType](implicit IsArrow: Match[T]) =
    new ArrowType[Cast.As[T, ?]] {
      def tarrow[T0, T1](t0: Cast.As[T, T0], t1: Cast.As[T, T1]) = new Cast.As[T, T0 => T1] {
        def apply[T2](t2: T[T2]): Option[(T0 => T1) Is T2] =
          for {
            result <- IsArrow.unapply(t2)
            eqT0   <- t0(result.is._1)
            eqT1   <- t1(result.is._2)
          } yield (eqT0, eqT1).lift2[Function1].andThen(result.is._3.flip)
      }
    }

  // implicit def ArrowTypeCast[T[_], T0, T1](
  //     implicit
  //     C1: Cast[T, T0],
  //     C2: Cast[T, T1],
  //     IsArrow: Match[T]
  // ) = new Cast[T, T0 => T1] {
  //   def apply[T2](t2: T[T2]): Option[(T0 => T1) Is T2] =
  //     for {
  //       result <- IsArrow.unapply(t2)
  //       eqT1   <- C1(result.is._1)
  //       eqT2   <- C2(result.is._2)
  //     } yield (eqT1, eqT2).lift2[Function1].andThen(result.is._3.flip)
  // }
}
