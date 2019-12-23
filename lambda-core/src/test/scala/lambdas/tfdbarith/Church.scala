package lambdas
package tfdbarith

import arithmetic.{ Arithmetic, Num }
import tfdb.Lambda

abstract class Church[Type[_]: ArrowType, E, T] {
  def apply[P[_, _]](implicit L: Lambda[Type, P], A: Forall[P, Arithmetic]): P[E, T]
}

object Church {

  implicit def ChurchArith[Type[_]: ArrowType]: Forall[Church[Type, ?, ?], Arithmetic] =
    new Forall[Church[Type, ?, ?], Arithmetic] {

      def apply[E] = new Arithmetic[Church[Type, E, ?]] {
        override def int(i: Num) = new Church[Type, E, Num] {
          def apply[P[_, _]](implicit L: Lambda[Type, P], A: Forall[P, Arithmetic]) =
            A[E].int(i)
        }

        override def abs(i: Church[Type, E, Num]): Church[Type, E, Num] =
          new Church[Type, E, Num] {
            def apply[P[_, _]](implicit L: Lambda[Type, P], A: Forall[P, Arithmetic]) =
              A[E].abs(i(L, A))
          }

        override def add(i1: Church[Type, E, Num])(i2: Church[Type, E, Num]) =
          new Church[Type, E, Num] {
            def apply[P[_, _]](implicit L: Lambda[Type, P], A: Forall[P, Arithmetic]) =
              A[E].add(i1(L, A))(i2(L, A))
          }

        override def mult(i1: Church[Type, E, Num])(i2: Church[Type, E, Num]) =
          new Church[Type, E, Num] {
            def apply[P[_, _]](implicit L: Lambda[Type, P], A: Forall[P, Arithmetic]) =
              A[E].mult(i1(L, A))(i2(L, A))
          }

        override def max(i1: Church[Type, E, Num])(i2: Church[Type, E, Num]): Church[Type, E, Num] =
          new Church[Type, E, Num] {
            def apply[P[_, _]](implicit L: Lambda[Type, P], A: Forall[P, Arithmetic]) =
              A[E].max(i1(L, A))(i2(L, A))
          }

        override def min(i1: Church[Type, E, Num])(i2: Church[Type, E, Num]): Church[Type, E, Num] =
          new Church[Type, E, Num] {
            def apply[P[_, _]](implicit L: Lambda[Type, P], A: Forall[P, Arithmetic]) =
              A[E].min(i1(L, A))(i2(L, A))
          }
      }
    }

  implicit def ChurchLambda[Type[_]: ArrowType]: Lambda[Type, Church[Type, ?, ?]] =
    new Lambda[Type, Church[Type, ?, ?]] {

      def vz[E, T: Type] = new Church[Type, (T, E), T] {
        def apply[P[_, _]](implicit L: Lambda[Type, P], A: Forall[P, Arithmetic]): P[(T, E), T] =
          L.vz
      }

      def vs[E, T: Type, T1: Type](a: Church[Type, E, T]) = new Church[Type, (T1, E), T] {
        def apply[P[_, _]](implicit L: Lambda[Type, P], A: Forall[P, Arithmetic]): P[(T1, E), T] =
          L.vs(a(L, A))
      }

      def lam[E, T1: Type, T2: Type](t: Church[Type, (T1, E), T2]) = new Church[Type, E, T1 => T2] {
        def apply[P[_, _]](implicit L: Lambda[Type, P], A: Forall[P, Arithmetic]): P[E, T1 => T2] =
          L.lam(t(L, A))
      }

      def app[E, T1: Type, T2: Type](f: Church[Type, E, T1 => T2])(t1: Church[Type, E, T1]) =
        new Church[Type, E, T2] {
          def apply[P[_, _]](implicit L: Lambda[Type, P], A: Forall[P, Arithmetic]): P[E, T2] =
            L.app(f(L, A))(t1(L, A))
        }
    }
}
