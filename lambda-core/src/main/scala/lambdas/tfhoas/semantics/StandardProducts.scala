package lambdas
package tfhoas
package semantics

object StandardProducts extends tfhoas.Products[cats.Id] {

  def tuple[A, B](a: A, b: B): (A, B) =
    (a, b)

  def fst[A, B](t: (A, B)): A =
    t._1

  def snd[A, B](t: (A, B)): B =
    t._2

}
