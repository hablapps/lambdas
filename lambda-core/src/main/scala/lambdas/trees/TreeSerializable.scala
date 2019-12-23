package lambdas
package trees

trait TreeSerializable[T[_]] {
  def show[A](t: T[A]): Tree
}

object TreeSerializable {
  def apply[T[_]](implicit T: TreeSerializable[T]) = T

  type ShowTree[A]     = Int => Tree
  type ShowTreeB[E, A] = Int => Tree
}
