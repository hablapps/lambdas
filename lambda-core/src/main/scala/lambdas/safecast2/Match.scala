package lambdas
package safecast2

trait Match[T[_], C[_]] {
  def unapply[A](t: T[A]): Option[C[A]]
}
