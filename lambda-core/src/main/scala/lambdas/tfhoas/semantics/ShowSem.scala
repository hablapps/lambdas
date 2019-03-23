package lambdas
package tfhoas
package semantics

object ShowLambda extends Lambda[Show] {

  def tuple[A, B](a: Show[A], b: Show[B]): Show[(A, B)] =
    (i: Int) => "(" + a(i) + ", " + b(i) + ").tuple"

  def fst[A, B](t: Show[(A, B)]): Show[A] =
    (i: Int) => t(i) + ".fst"

  def snd[A, B](t: Show[(A, B)]): Show[B] =
    (i: Int) => t(i) + ".snd"

  def tupled[A, B, C](f: Show[(A, B) => C]): Show[((A, B)) => C] =
    (i: Int) => f(i) + ".tupled"

  def curried[A, B, C](f: Show[(A, B) => C]): Show[A => B => C] =
    (i: Int) => f(i) + ".curried"

  def lam[A, B](f: Show[A] => Show[B]): Show[A => B] =
    (i: Int) => {
      val x = "x" + i
      "(λ" + x + "." + f((_: Int) => x)(i + 1) + ")"
    }

  def lam2[A, B, C](f: (Show[A], Show[B]) => Show[C]): Show[(A, B) => C] =
    (i: Int) => {
      val xi = "x" + i
      val xj = "x" + i + 1
      "(λ(" + xi + ", " + xj + ")." + f((_: Int) => xi, (_: Int) => xj)(i + 2) + ")"
    }

  def app[A, B](f: Show[A => B])(a: Show[A]): Show[B] =
    (i: Int) => "(" + f(i) + " " + a(i) + ")"
}
