package lambdas
package deserialization

import scalaz.Leibniz._

abstract class AsArrow[A]{
  type T1
  type T2
  val tA: TQ[A]
  // val eq: Option[(TQ[T1], TQ[T2], A === (T1 => T2))]

  def cast[F[_]](pa: F[A]): Option[(TQ[T1], TQ[T2], F[T1 => T2])]
    // eq.map{ eq => (eq._1, eq._2, eq._3.subst[F](pa)) }
}

object AsArrow{

  // def unapply[A](asA: AsArrow[A]): Option[(TQ[A], Option[(TQ[asA.T1], TQ[asA.T2], A === (asA.T1 => asA.T2))])] =
  //   Some((asA.tA, asA.eq))

  // def apply[A, _T1, _T2](_tA: TQ[A],
  //     _eq: Option[(TQ[_T1], TQ[_T2], A === (_T1 => _T2))]) = new AsArrow[A]{
  //   type T1 = _T1
  //   type T2 = _T2
  //   val tA = _tA
  //   val eq = _eq
  // }

  implicit val AsArrowTSYM = new TSYM[AsArrow]{
    def tint: AsArrow[Int] = new AsArrow[Int]{
      type T1 = Nothing
      type T2 = Nothing
      val tA = TSYM[TQ].tint
      def cast[F[_]](pa: F[Int]): Option[(TQ[T1], TQ[T2], F[T1 => T2])] =
        None
    }

    def tarr[_T1, _T2](t1: AsArrow[_T1], t2: AsArrow[_T2]) = new AsArrow[_T1 => _T2]{
      type T1 = _T1
      type T2 = _T2
      val tA = t1.tA -> t2.tA
      def cast[F[_]](pa: F[T1 => T2]): Option[(TQ[T1], TQ[T2], F[T1 => T2])] =
        Some((t1.tA, t2.tA, refl.subst[F](pa)))
    }
  }
}
