package lambdas
package taglessfinal
package hoas

trait Lambda[P[_]]{

  def int(i: Int): P[Int]

  def add(i1: P[Int])(i2: P[Int]): P[Int]

  def lam[T1, T2](f: P[T1] => P[T2]): P[T1 => T2]

  def app[T1, T2](f: P[T1 => T2])(t1: P[T1]): P[T2]
}

object Lambda{

  def apply[P[_]](implicit L: Lambda[P]) = L

  implicit val ShowSem: Lambda[ShowH] = semantics.ShowLambda

  implicit val StdSem: Lambda[cats.Id] = semantics.Std
}
