package lambdas
package tfhoas
package semantics

class ShowLambda[Type[_]: ArrowType] extends Lambda[Type, Show] {

  def lam[A: Type, B: Type](f: Show[A] => Show[B]): Show[A => B] =
    (i: Int) => {
      val x = "x" + i
      "(λ" + x + "." + f((_: Int) => x)(i + 1) + ")"
    }

  def app[A: Type, B: Type](f: Show[A => B])(a: Show[A]): Show[B] =
    (i: Int) => "(" + f(i) + " " + a(i) + ")"

  def tupled[A: Type, B: Type, C: Type](f: Show[(A, B) => C]): Show[((A, B)) => C] =
    (i: Int) => f(i) + ".tupled"

  def curried[A: Type, B: Type, C: Type](f: Show[(A, B) => C]): Show[A => B => C] =
    (i: Int) => f(i) + ".curried"

  def lam2[A: Type, B: Type, C: Type](f: (Show[A], Show[B]) => Show[C]): Show[(A, B) => C] =
    (i: Int) => {
      val xi = "x" + i
      val xj = "x" + i + 1
      "(λ(" + xi + ", " + xj + ")." + f((_: Int) => xi, (_: Int) => xj)(i + 2) + ")"
    }
}
