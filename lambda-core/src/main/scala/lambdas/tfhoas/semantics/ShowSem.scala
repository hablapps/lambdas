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

  def tuple[A: Type, B: Type](a: Show[A], b: Show[B]): Show[(A, B)] =
    (i: Int) => "(" + a(i) + ", " + b(i) + ").tuple"

  def fst[A: Type, B: Type](t: Show[(A, B)]): Show[A] =
    (i: Int) => t(i) + ".fst"

  def snd[A: Type, B: Type](t: Show[(A, B)]): Show[B] =
    (i: Int) => t(i) + ".snd"

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
