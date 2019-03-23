package lambdas
package safecast

abstract class ATypeTerm[T[_]] {
  type A
  val typ: T[A]
}

object ATypeTerm {

  import scala.language.implicitConversions

  implicit def apply[T[_], _A](t: T[_A]): ATypeTerm[T] = new ATypeTerm[T] {
    type A = _A
    val typ = t
  }
}
