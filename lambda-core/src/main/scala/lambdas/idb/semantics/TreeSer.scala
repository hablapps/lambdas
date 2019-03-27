package lambdas
package idb
package semantics

import trees._
import safecast.Typeable

trait TreeSer[E, T, L <: Lambda[E, T, L]] {
  def apply(l: Lambda[E, T, L]): Int => Tree
}

object TreeSer {

  def apply[E, T, L <: Lambda[E, T, L]](
      l: Lambda[E, T, L]
  )(implicit S: TreeSer[E, T, L]): Int => Tree =
    S(l)

  import Constructors._

  implicit def _Vz[E, T] = new TreeSer[(T, E), T, Vz[E, T]] {
    def apply(l: Lambda[(T, E), T, Vz[E, T]]): Int => Tree =
      i => {
        val n = if (i - 1 < 0) "y" else "x"
        val j = (i - 1).abs
        tr_vr(s"$n$j")
      }
  }

  implicit def _Vs[E, T, T1, L <: Lambda[E, T, L]](implicit S: TreeSer[E, T, L]) =
    new TreeSer[(T1, E), T, Vs[E, T, T1, L]] {
      def apply(l: Lambda[(T1, E), T, Vs[E, T, T1, L]]): Int => Tree =
        i =>
          l match {
            case Vs(a) => S(a)(i - 1)
          }
    }

  implicit def _Lam[E, T1, T2, L <: Lambda[(T1, E), T2, L], Type[_]](
      implicit
      S: TreeSer[(T1, E), T2, L],
      Type1: shapeless.Lazy[Typeable.Aux[T1, Type]],
      Ser: Treeable[Type]
  ) =
    new TreeSer[E, T1 => T2, Lam[E, T1, T2, L]] {
      def apply(l: Lambda[E, T1 => T2, Lam[E, T1, T2, L]]): Int => Tree =
        i =>
          l match {
            case Lam(body) => tr_lam(s"x$i", Ser.show(Type1.value.T), S(body)(i + 1))
          }
    }

  implicit def _App[E, T1, T2, Lf <: Lambda[E, T1 => T2, Lf], L1 <: Lambda[E, T1, L1]](
      implicit
      Sf: TreeSer[E, T1 => T2, Lf],
      S1: TreeSer[E, T1, L1]
  ) = new TreeSer[E, T2, App[E, T1, T2, Lf, L1]] {
    def apply(l: Lambda[E, T2, App[E, T1, T2, Lf, L1]]): Int => Tree =
      i =>
        l match {
          case App(f, t1) => tr_app(Sf(f)(i), S1(t1)(i))
        }
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
