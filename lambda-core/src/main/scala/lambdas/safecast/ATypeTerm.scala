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
