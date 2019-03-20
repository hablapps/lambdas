package lambdas
package safecast2

import cats.evidence._

trait IntArrowType[T[_]] {
  def tint: T[Int]
  def tarrow[T1, T2](t1: T[T1], t2: T[T2]): T[T1 => T2]
}

object IntArrowType {

  def apply[T[_]](implicit A: IntArrowType[T]) = A

  abstract class Cases[W[_], T[_], A] {
    def isInt(ta: T[A], is: A Is Int): W[A]
    def isArrow(ta: T[A], is: { type T1; type T2; val is: A Is (T1 => T2) }): W[A]
  }

  trait Match[T[_], A] {
    def apply[W[_]](
        isInt: (T[A], IntType.Case[A]) => W[A],
        isArrow: ArrowType.Case[T, A] => W[A]
    ): W[A]
  }

  object Match {

    implicit def Match_IntArrowType[T[_]: IntArrowType] = new IntArrowType[Match[T, ?]] {

      def tint = new Match[T, Int] {
        def apply[W[_]](
            isInt: (T[Int], IntType.Case[Int]) => W[Int],
            isArrow: ArrowType.Case[T, Int] => W[Int]
        ): W[Int] =
          isInt(IntArrowType[T].tint, IntType.Case(Is.refl[Int]))
      }

      def tarrow[_T1, _T2](t1: Match[T, _T1], t2: Match[T, _T2]) = new Match[T, _T1 => _T2] {
        def apply[W[_]](
            isInt: (T[_T1 => _T2], IntType.Case[_T1 => _T2]) => W[_T1 => _T2],
            isArrow: ArrowType.Case[T, _T1 => _T2] => W[_T1 => _T2]
        ): W[_T1 => _T2] =
          isArrow(
            new ArrowType.Case[T, _T1 => _T2] {
              type T1 = _T1
              type T2 = _T2
              val ta = IntArrowType[T].tarrow(t1[T]((t, _) => t, _.ta), t2[T]((t, _) => t, _.ta))
              val is = (t1[T]((t, _) => t, _.ta), t2[T]((t, _) => t, _.ta), Is.refl[_T1 => _T2])
            }
          )
      }
    }
  }

  case class IntMatch[T[_], A](t: T[A], isInt: Option[A Is Int])

  object IntMatch {

    def IntMatch_IntArrowType[T[_]: IntArrowType] = new IntArrowType[IntMatch[T, ?]] {
      def tint: IntMatch[T, Int] =
        IntArrowType[Match[T, ?]].tint.apply[IntMatch[T, ?]](
          (ta, isInt) => IntMatch(ta, Some(isInt.is)),
          isArrow => IntMatch(isArrow.ta, None)
        )

      def tarrow[T1, T2](t1: IntMatch[T, T1], t2: IntMatch[T, T2]): IntMatch[T, T1 => T2] =
        IntMatch(IntArrowType[T].tarrow(t1.t, t2.t), None)
    }
  }
}
