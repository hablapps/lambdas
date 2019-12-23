package lambdas
package tfhoas
package semantics

object ShowProducts extends Products[Show] {

  def tuple[A, B](a: Show[A], b: Show[B]): Show[(A, B)] =
    (i: Int) => "(" + a(i) + ", " + b(i) + ").tuple"

  def fst[A, B](t: Show[(A, B)]): Show[A] =
    (i: Int) => t(i) + ".fst"

  def snd[A, B](t: Show[(A, B)]): Show[B] =
    (i: Int) => t(i) + ".snd"

}
