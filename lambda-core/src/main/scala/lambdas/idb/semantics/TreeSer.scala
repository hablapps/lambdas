package lambdas
package idb
package semantics

import trees._
import safecast.Typeable

trait TreeSer[E, T, L <: Lambda[E, T]] {
  def apply(l: L): Int => Tree
}

object TreeSer {

  def apply[E, T, L <: Lambda[E, T]](l: L)(implicit S: TreeSer[E, T, L]): Int => Tree =
    S(l)

  import Constructors._

  implicit def _Vz[E, T] = new TreeSer[(T, E), T, Vz[E, T]] {
    def apply(l: Vz[E, T]): Int => Tree =
      i => {
        val n = if (i - 1 < 0) "y" else "x"
        val j = (i - 1).abs
        tr_vr(s"$n$j")
      }
  }

  implicit def _Vs[E, T, T1, L <: Lambda[E, T]](implicit S: TreeSer[E, T, L]) =
    new TreeSer[(T1, E), T, Vs[E, T, T1, L]] {
      def apply(l: Vs[E, T, T1, L]): Int => Tree =
        i => S(l.a)(i - 1)
    }

  implicit def _Lam[E, T1, T2, L <: Lambda[(T1, E), T2], Type[_]](
      implicit
      S: TreeSer[(T1, E), T2, L],
      Type1: shapeless.Lazy[Typeable.Aux[T1, Type]],
      Ser: Treeable[Type]
  ) =
    new TreeSer[E, T1 => T2, Lam[E, T1, T2, L]] {
      def apply(l: Lam[E, T1, T2, L]): Int => Tree =
        i => tr_lam(s"x$i", Ser.show(Type1.value.T), S(l.body)(i + 1))
    }

  implicit def _App[E, T1, T2, Lf <: Lambda[E, T1 => T2], L1 <: Lambda[E, T1]](
      implicit
      Sf: TreeSer[E, T1 => T2, Lf],
      S1: TreeSer[E, T1, L1]
  ) = new TreeSer[E, T2, App[E, T1, T2, Lf, L1]] {
    def apply(l: App[E, T1, T2, Lf, L1]): Int => Tree =
      i => tr_app(Sf(l.f)(i), S1(l.t1)(i))
  }

  object Constructors {

    def tr_vr(name: String): Tree =
      Node("Var", List(Leaf(name)))

    def tr_lam(name: String, typ: Tree, body: Tree): Tree =
      Node("Lam", List(Leaf(name), typ, body))

    def tr_app(f: Tree, a: Tree): Tree =
      Node("App", List(f, a))

    def tr_tArr(t1: Tree, t2: Tree): Tree =
      Node("TArr", List(t1, t2))
  }

}
//   def app[E, T1, T2](f: Int => String)(t1: Int => String): Int => String =
//     i => "(" + f(i) + " " + t1(i) + ")"
// }
