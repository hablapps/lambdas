package lambdas
package tfhoas

trait Lambda[P[_]] {

  def lam[T1, T2](f: P[T1] => P[T2]): P[T1 => T2]

  def app[T1, T2](f: P[T1 => T2])(t1: P[T1]): P[T2]

  // Products

  def tuple[A, B](a: P[A], b: P[B]): P[(A, B)]

  def fst[A, B](t: P[(A, B)]): P[A]

  def snd[A, B](t: P[(A, B)]): P[B]

  // Auxiliary

  def lam2[A, B, C](f: (P[A], P[B]) => P[C]): P[(A, B) => C]

  def curried[A, B, C](f: P[(A, B) => C]): P[A => B => C]

  def tupled[A, B, C](f: P[(A, B) => C]): P[((A, B)) => C]
}

object Lambda {

  def apply[P[_]](implicit L: Lambda[P]) = L

  implicit val ShowSem: Lambda[Show]   = semantics.ShowLambda
  implicit val StdSem: Lambda[cats.Id] = semantics.Std
}
