package lambdas
package trees

trait Treeable[T[_]] {
  def show[A](t: T[A]): Tree
}

object Treeable {
  def apply[T[_]](implicit T: Treeable[T]) = T

  type ShowTree[A] = Int => Tree
}
