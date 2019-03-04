package lambdas
package deserialization

trait TSYM[P[_]]{
  def tint: P[Int]
  def tarr[T1, T2](t1: P[T1], t2: P[T2]): P[T1 => T2]
}

object TSYM{

  def apply[P[_]](implicit T: TSYM[P]) = T

  trait Syntax{
    def tint[P[_]](implicit T: TSYM[P]): P[Int] =
      T.tint

    implicit class TArrOp[P[_], T1](t1: P[T1])(implicit T: TSYM[P]){
      def ->[T2](t2: P[T2]): P[T1 => T2] =
        T.tarr(t1, t2)
    }
  }
}
