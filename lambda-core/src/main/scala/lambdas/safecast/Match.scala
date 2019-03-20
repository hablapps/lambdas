package lambdas
package safecast

trait Match[T[_], C[_]] {
  def unapply[A](t: T[A]): Option[C[A]]
}
