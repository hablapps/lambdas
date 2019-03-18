package lambdas
package safecast

abstract class ATypeTerm {
  type A
  val typ: TypeTerm[A]
}

object ATypeTerm {
  implicit def apply[_A](t: TypeTerm[_A]): ATypeTerm = new ATypeTerm {
    type A = _A
    val typ = t
  }
}

abstract class ATypeTerm2[T[_]] {
  type A
  val typ: T[A]
}

object ATypeTerm2 {
  implicit def apply[T[_], _A](t: T[_A]): ATypeTerm2[T] = new ATypeTerm2[T] {
    type A = _A
    val typ = t
  }
}
