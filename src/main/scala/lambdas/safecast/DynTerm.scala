package lambdas
package safecast

abstract class DynTerm[F[_]] {
  type A
  val typ: TypeTerm[A]
  val term: F[A]

  def as[B](tb: TypeTerm[B]): Option[F[B]] =
    typ[Cast].as[B, F](tb, term)

  def asInt: Option[F[Int]] =
    as(tint[TypeTerm])

  abstract class _AsArrow {
    type T1
    type T2
    val typ1: TypeTerm[T1]
    val typ2: TypeTerm[T2]
    val term: F[T1 => T2]
  }

  def asArrow: Option[_AsArrow] = {
    val asArrow = typ[CastArrow]
    asArrow.as[F](term) map {
      case (_typ1, _typ2, _term) =>
        new _AsArrow {
          type T1 = asArrow.T1
          type T2 = asArrow.T2
          val typ1 = _typ1
          val typ2 = _typ2
          val term = _term
        }
    }
  }
}

object DynTerm {

  def unapply[F[_]](dt: DynTerm[F]): Option[(TypeTerm[_], F[_])] =
    Some((dt.typ, dt.term))

  def apply[F[_], _A](_typ: TypeTerm[_A], _term: F[_A]): DynTerm[F] =
    new DynTerm[F] {
      type A = _A
      val typ  = _typ
      val term = _term
    }
}
