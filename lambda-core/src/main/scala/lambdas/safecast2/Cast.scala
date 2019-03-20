package lambdas
package safecast2

import cats.evidence._

trait Cast[T[_]] {
  def apply[T1, T2](t1: T[T1], t2: T[T2]): Option[T1 Is T2]

  def as[T1, T2, F[_]](t1: T[T1], t2: T[T2])(f1: F[T1]): Option[F[T2]] =
    apply(t1, t2) map (_.substitute[F](f1))
}

object Cast {
  abstract class As[T[_], T1] {
    def apply[T2](t2: T[T2]): Option[T1 Is T2]
  }
}
