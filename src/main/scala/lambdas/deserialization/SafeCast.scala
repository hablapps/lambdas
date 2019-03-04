package lambdas
package deserialization

import scalaz.Leibniz._

abstract class SafeCast[A]{
  def apply[B](tb: TQ[B]): Option[A === B]
}

object SafeCast{

  implicit def apply[A, B, E, P[_, _]](sc: SafeCast[A]): (TQ[B], P[E, A]) => Option[P[E, B]] =
    (tb, pa) => sc(tb).map(_.subst[P[E, ?]](pa))

  implicit val SafeCastTSYM = new TSYM[SafeCast]{
    def tint = new SafeCast[Int]{
      def apply[B](tb: TQ[B]) =
        tb[AsInt].eq map symm
    }

    def tarr[T1, T2](t1: SafeCast[T1], t2: SafeCast[T2]) = new SafeCast[T1 => T2]{
      def apply[B](tb: TQ[B]) = {
        val asArr = tb[AsArrow]
        for {
          value <- asArr.eq
          eqT1 <- t1(value._1)
          eqT2 <- t2(value._2)
        } yield lift2[Nothing, Nothing, Nothing, Any, Any, Any, Function1, T1, asArr.T1, T2, asArr.T2](
          eqT1, eqT2).andThen(symm[Nothing, Any, B, asArr.T1 => asArr.T2](value._3))
      }
    }
  }
}
