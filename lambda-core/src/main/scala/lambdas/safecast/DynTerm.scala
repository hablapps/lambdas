package lambdas
package safecast

abstract class DynTerm[T[_], F[_]] {
  type A
  val typ: T[A]
  val term: F[A]

  def as[B](tb: T[B])(implicit C: Cast[T]): Option[F[B]] =
    C.as[A, B, F](typ, tb)(term)

  def as1[B](implicit tb: T[B], C: Cast[T]): Option[F[B]] =
    C.as[A, B, F](typ, tb)(term)
}

object DynTerm {

  type Aux[T[_], F[_], _A] = DynTerm[T, F] { type A = _A }

  def unapply[T[_], F[_]](dt: DynTerm[T, F]): Option[(T[_], F[_])] =
    Some((dt.typ, dt.term))

  def apply[T[_], F[_], _A](_typ: T[_A], _term: F[_A]): DynTerm[T, F] =
    new DynTerm[T, F] {
      type A = _A
      val typ  = _typ
      val term = _term
    }
}
