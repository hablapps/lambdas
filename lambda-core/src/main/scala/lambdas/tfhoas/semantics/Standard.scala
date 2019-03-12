package lambdas
package tfhoas
package semantics

object Std extends Lambda[cats.Id] {

  def int(i: Int): Int = i

  def add(i1: Int)(i2: Int): Int = i1 + i2

  def tuple[A, B](a: A, b: B): (A, B) = (a, b)

  def fst[A, B](t: (A, B)): A = t._1

  def snd[A, B](t: (A, B)): B = t._2

  def tupled[A, B, C](f: (A, B) => C): ((A, B)) => C = f.tupled

  def curried[A, B, C](f: (A, B) => C): A => B => C = f.curried

  def lam[A, B](f: A => B): A => B = f

  def lam2[A, B, C](f: (A, B) => C): (A, B) => C = f

  def app[A, B](f: A => B)(a: A): B = f(a)
}
