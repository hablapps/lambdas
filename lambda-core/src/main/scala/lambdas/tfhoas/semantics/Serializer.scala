package lambdas
package tfhoas
package semantics

import safecast.TypeSummoner
import trees._, Treeable.ShowTree

class Serializer[Type[_]: ArrowType: Treeable]
    extends tfhoas.Lambda[Type, ShowTree]
    with TypeSummoner[Type] {

  import Serializer.Constructors._

  def lam[A: Type, B: Type](f: (Int => Tree) => (Int => Tree)): Int => Tree =
    (i: Int) => {
      val x = "x" + i
      tr_lam(
        x,
        Treeable[Type].show(Type[A]),
        f((_: Int) => tr_vr(x))(i + 1)
      )
    }

  def app[A: Type, B: Type](
      f: Int => Tree
  )(a: Int => Tree): Int => Tree =
    (i: Int) => tr_app(f(i), a(i))

  // Products

  def tuple[A: Type, B: Type](a: ShowTree[A], b: ShowTree[B]): ShowTree[(A, B)] =
    i => tr_tuple(a(i), b(i))

  def fst[A: Type, B: Type](t: ShowTree[(A, B)]): ShowTree[A] =
    i => tr_fst(t(i))

  def snd[A: Type, B: Type](t: ShowTree[(A, B)]): ShowTree[B] =
    i => tr_snd(t(i))

  // Auxiliary

  def lam2[A: Type, B: Type, C: Type](
      f: (ShowTree[A], ShowTree[B]) => ShowTree[C]
  ): ShowTree[(A, B) => C] =
    (i: Int) => {
      val x = "x" + i
      val y = "x" + i + 1
      tr_lam2(
        x,
        Treeable[Type].show(Type[A]),
        y,
        Treeable[Type].show(Type[B]),
        f((_: Int) => tr_vr(x), (_: Int) => tr_vr(y))(i + 2)
      )
    }

  def curried[A: Type, B: Type, C: Type](f: ShowTree[(A, B) => C]): ShowTree[A => B => C] =
    f andThen tr_curried

  def tupled[A: Type, B: Type, C: Type](f: ShowTree[(A, B) => C]): ShowTree[((A, B)) => C] =
    f andThen tr_tupled
}

object Serializer {
  trait Constructors {

    def tr_vr(name: String): Tree =
      Node("Var", List(Leaf(name)))

    def tr_lam(name: String, typ: Tree, body: Tree): Tree =
      Node("Lam", List(Leaf(name), typ, body))

    def tr_app(f: Tree, a: Tree): Tree =
      Node("App", List(f, a))

    def tr_tuple(fst: Tree, snd: Tree): Tree =
      Node("Tuple2", List(fst, snd))

    def tr_fst(t: Tree): Tree =
      Node("Fst", List(t))

    def tr_snd(t: Tree): Tree =
      Node("Snd", List(t))

    def tr_lam2(name1: String, typ1: Tree, name2: String, typ2: Tree, body: Tree): Tree =
      Node("Lam2", List(Leaf(name1), typ1, Leaf(name2), typ2, body))

    def tr_curried(t: Tree): Tree =
      Node("Curry", List(t))

    def tr_tupled(t: Tree): Tree =
      Node("Tupled", List(t))
  }

  object Constructors extends Constructors
}
