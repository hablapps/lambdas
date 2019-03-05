package lambdas
package deserialization

import scalaz.Leibniz._

abstract class As[A]{
  def apply[B](tb: TQ[B]): Option[A === B]

  def cast[B, F[_]](tb: TQ[B], fa: F[A]): Option[F[B]] =
    apply(tb).map(_.subst[F](fa))
}

object As{

  implicit val AsTSYM = new TSYM[As]{
    def tint = new As[Int]{
      def apply[B](tb: TQ[B]) =
        tb[AsInt].eq map symm
    }

    def tarr[T1, T2](t1: As[T1], t2: As[T2]) = new As[T1 => T2]{
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
