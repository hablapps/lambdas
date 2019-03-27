package lambdas
package safecast

trait TypeSummoner[T[_]] {
  def Type[A](implicit T: T[A]) = T
}
