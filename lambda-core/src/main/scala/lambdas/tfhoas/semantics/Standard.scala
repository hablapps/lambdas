package lambdas
package tfhoas
package semantics

class Standard[Type[_]: ArrowType] extends tfhoas.Lambda[Type, cats.Id] {

  def lam[A: Type, B: Type](f: A => B): A => B =
    f

  def app[A: Type, B: Type](f: A => B)(t1: A): B =
    f(t1)

  // Auxiliary

  def lam2[A: Type, B: Type, C: Type](f: (A, B) => C): (A, B) => C =
    f

  def curried[A: Type, B: Type, C: Type](f: (A, B) => C): A => B => C =
    f.curried

  def tupled[A: Type, B: Type, C: Type](f: (A, B) => C): ((A, B)) => C =
    f.tupled

}

object Lambda {

  def apply[Type[_], P[_]](implicit L: Lambda[Type, P]) = L
}
