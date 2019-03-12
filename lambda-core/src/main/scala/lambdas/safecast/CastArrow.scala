package lambdas
package safecast

import cats.evidence._

abstract class CastArrow[A] {
  type T1
  type T2
  val tA: TypeTerm[A]
  val eq: Option[(TypeTerm[T1], TypeTerm[T2], A Is (T1 => T2))]

  def as[F[_]](pa: F[A]): Option[(TypeTerm[T1], TypeTerm[T2], F[T1 => T2])] =
    eq.map { eq =>
      (eq._1, eq._2, eq._3.substitute[F](pa))
    }
}

object CastArrow {

  def apply[A, _T1, _T2](
      _tA: TypeTerm[A],
      _eq: Option[(TypeTerm[_T1], TypeTerm[_T2], A Is (_T1 => _T2))]
  ) =
    new CastArrow[A] {
      type T1 = _T1
      type T2 = _T2
      val tA = _tA
      val eq = _eq
    }

  implicit val CastArrowType = new Type[CastArrow] {
    def tint: CastArrow[Int] =
      CastArrow[Int, Nothing, Nothing](Type[TypeTerm].tint, None)

    def tarr[T1, T2](t1: CastArrow[T1], t2: CastArrow[T2]): CastArrow[T1 => T2] =
      CastArrow[T1 => T2, T1, T2](t1.tA -> t2.tA, Some((t1.tA, t2.tA, Is.refl)))
  }
}
