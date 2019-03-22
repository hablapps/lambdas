package lambdas
package tfdb

trait Lambda[P[E, T]] {
  def int[E](i: Int): P[E, Int]
  def add[E](i1: P[E, Int], i2: P[E, Int]): P[E, Int]
  def vz[E, T]: P[(T, E), T]
  def vs[E, T, T1](a: P[E, T]): P[(T1, E), T]
  def lam[E, T1, T2](t: P[(T1, E), T2]): P[E, T1 => T2]
  def app[E, T1, T2](f: P[E, T1 => T2])(t1: P[E, T1]): P[E, T2]
}

object Lambda {
  def apply[P[E, T]](implicit L: Lambda[P]) = L

  implicit val ShowSem: Lambda[ShowB]                        = semantics.ShowSem
  implicit val StdSem: Lambda[Function1]                     = semantics.Standard
  implicit val TermSem: Lambda[semantics.Term]               = semantics.Term.TermLambda
  implicit def TupledSem[P1[_, _]: Lambda, P2[_, _]: Lambda] = new semantics.TupledInstance[P1, P2]
}
