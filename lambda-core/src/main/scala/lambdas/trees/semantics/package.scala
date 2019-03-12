package lambdas
package trees

package object semantics extends DynLTermModule {

  type Semantics[T] = Tree => T

  implicit class EitherOp[A](o: Option[A]) {
    def toEither[B](none: B): Either[B, A] =
      o.fold[Either[B, A]](Left(none))(Right(_))
  }
}
