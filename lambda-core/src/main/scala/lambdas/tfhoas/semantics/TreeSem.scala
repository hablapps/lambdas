package lambdas
package tfhoas
package semantics

import trees._
import tfdb.ArrowType

abstract class ShowTree[T[_], A] extends (Int => (Tree, T[A]))

object ShowTree {

  def apply[T[_], A](f: Int => (Tree, T[A])) = new ShowTree[T, A] {
    def apply(i: Int) = f(i)
  }

  import Constructors._

  implicit def _Lambda[T[_]: ArrowType] = new tfhoas.Lambda[ShowTree[T, ?]] {

    def tuple[A, B](a: ShowTree[T, A], b: ShowTree[T, B]): ShowTree[T, (A, B)] =
      ??? //      (i: Int) => (tr_tuple(a(i)._1, b(i)._1), ???)

    def fst[A, B](t: ShowTree[T, (A, B)]): ShowTree[T, A] =
      ??? //      (i: Int) => (tr_fst(t(i)._1), ???)

    def snd[A, B](t: ShowTree[T, (A, B)]): ShowTree[T, B] =
      ??? //      (i: Int) => (tr_snd(t(i)._1), ???)

    def tupled[A, B, C](f: ShowTree[T, (A, B) => C]): ShowTree[T, ((A, B)) => C] =
      ??? //      (i: Int) => (tr_tupled(f(i)._1), ???)

    def curried[A, B, C](f: ShowTree[T, (A, B) => C]): ShowTree[T, A => B => C] =
      ??? //      (i: Int) => (tr_curried(f(i)._1), ???)

    def lam[A, B](f: ShowTree[T, A] => ShowTree[T, B]): ShowTree[T, A => B] =
      ??? //      (i: Int) => {
    //   val x = "x" + i
    //   (tr_lam(x, Leaf("unknown"), f((_: Int) => (tr_vr(x), ???))(i + 1)._1), ???)
    // }

    def lam2[A, B, C](
        f: (ShowTree[T, A], ShowTree[T, B]) => ShowTree[T, C]
    ): ShowTree[T, (A, B) => C] =
      ???
    // (i: Int) => {
    //   val xi = "x" + i
    //   val xj = "x" + i + 1
    //   (
    //     tr_lam2(
    //       xi,
    //       xj,
    //       Leaf("unknown1"),
    //       Leaf("unknown2"),
    //       f((_: Int) => (tr_vr(xi), ???), (_: Int) => (tr_vr(xj), ???))(i + 2)._1
    //     ),
    //     ???
    //   )
    // }

    def app[A, B](f: ShowTree[T, A => B])(a: ShowTree[T, A]): ShowTree[T, B] =
      ??? //      (i: Int) => (tr_app(f(i)._1, a(i)._1), ???)
  }

  object Constructors {

    def tr_tuple(a: Tree, b: Tree): Tree =
      Node("Tuple", List(a, b))

    def tr_fst(t: Tree): Tree =
      Node("First", List(t))

    def tr_snd(t: Tree): Tree =
      Node("Second", List(t))

    def tr_tupled(f: Tree): Tree =
      Node("Tupled", List(f))

    def tr_curried(f: Tree): Tree =
      Node("Curried", List(f))

    def tr_vr(name: String): Tree =
      Node("Var", List(Leaf(name)))

    def tr_lam(name: String, typ: Tree, body: Tree): Tree =
      Node("Lam", List(Leaf(name), typ, body))

    def tr_lam2(name1: String, name2: String, typ1: Tree, typ2: Tree, body: Tree): Tree =
      Node("Lam2", List(Leaf(name1), typ1, Leaf(name2), typ2, body))

    def tr_app(f: Tree, a: Tree): Tree =
      Node("App", List(f, a))

    def tr_tArr(t1: Tree, t2: Tree): Tree =
      Node("TArr", List(t1, t2))
  }
}
