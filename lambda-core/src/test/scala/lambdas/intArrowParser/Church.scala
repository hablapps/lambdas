package lambdas
package intArrowParser

import arithmetic.Arithmetic
import tfdb.Lambda

trait Church[E, T] {
  def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[E, T]
}

object Church {

  implicit object ChurchArith extends ForAll[Church, Arithmetic] {

    def apply[E] = new Arithmetic[Church[E, ?]] {
      def int(i: Int) = new Church[E, Int] {
        def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]) =
          A[E].int(i)
      }

      def add(i1: Church[E, Int])(i2: Church[E, Int]) =
        new Church[E, Int] {
          def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]) =
            A[E].add(i1(L, A))(i2(L, A))
        }

      def * = new Church[E, (Int, Int) => Int] {
        def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]) =
          A[E].*
      }

      def + = new Church[E, (Int, Int) => Int] {
        def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]) =
          A[E].+
      }
    }
  }

  implicit object ChurchLambda extends Lambda[Church] {

    def int[E](i: Int) = new Church[E, Int] {
      def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[E, Int] =
        A[E].int(i)
    }

    def add[E](i1: Church[E, Int], i2: Church[E, Int]) =
      new Church[E, Int] {
        def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[E, Int] =
          A[E].add(i1(L, A))(i2(L, A))
      }

    def vz[E, T] = new Church[(T, E), T] {
      def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[(T, E), T] =
        L.vz
    }

    def vs[E, T, T1](a: Church[E, T]) = new Church[(T1, E), T] {
      def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[(T1, E), T] =
        L.vs(a(L, A))
    }

    def lam[E, T1, T2](t: Church[(T1, E), T2]) = new Church[E, T1 => T2] {
      def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[E, T1 => T2] =
        L.lam(t(L, A))
    }

    def app[E, T1, T2](f: Church[E, T1 => T2])(t1: Church[E, T1]) =
      new Church[E, T2] {
        def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[E, T2] =
          L.app(f(L, A))(t1(L, A))
      }
  }
}
