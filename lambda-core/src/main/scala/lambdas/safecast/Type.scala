package lambdas
package safecast

trait Type[P[_]] {
  def tint: P[Int]
  def tarr[T1, T2](t1: P[T1], t2: P[T2]): P[T1 => T2]
}

object Type {

  def apply[P[_]](implicit T: Type[P]) = T

  implicit object ShowType extends Type[λ[T => String]] {
    def tint: String                         = "Int"
    def tarr[T1, T2](t1: String, t2: String) = s"($t1 => $t2)"
  }

  trait Syntax {
    def tint[P[_]](implicit T: Type[P]): P[Int] =
      T.tint

    implicit class TArrOp[P[_], T1](t1: P[T1])(implicit T: Type[P]) {
      def ->[T2](t2: P[T2]): P[T1 => T2] =
        T.tarr(t1, t2)
    }
  }
}
