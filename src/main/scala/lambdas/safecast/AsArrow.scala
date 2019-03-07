package lambdas
package safecast

import scalaz.Leibniz._

abstract class AsArrow[A] {
  type T1
  type T2
  val tA: TypeTerm[A]
  val eq: Option[(TypeTerm[T1], TypeTerm[T2], A === (T1 => T2))]

  def as[F[_]](pa: F[A]): Option[(TypeTerm[T1], TypeTerm[T2], F[T1 => T2])] =
    eq.map { eq =>
      (eq._1, eq._2, eq._3.subst[F](pa))
    }
}

object AsArrow {

  def apply[A, _T1, _T2](
      _tA: TypeTerm[A],
      _eq: Option[(TypeTerm[_T1], TypeTerm[_T2], A === (_T1 => _T2))]
  ) =
    new AsArrow[A] {
      type T1 = _T1
      type T2 = _T2
      val tA = _tA
      val eq = _eq
    }

  implicit val AsArrowType = new Type[AsArrow] {
    def tint: AsArrow[Int] =
      AsArrow[Int, Nothing, Nothing](Type[TypeTerm].tint, None)

    def tarr[T1, T2](t1: AsArrow[T1], t2: AsArrow[T2]): AsArrow[T1 => T2] =
      AsArrow[T1 => T2, T1, T2](t1.tA -> t2.tA, Some((t1.tA, t2.tA, refl)))
  }
}
