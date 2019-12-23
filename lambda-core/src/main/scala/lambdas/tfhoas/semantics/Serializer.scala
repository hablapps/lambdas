package lambdas
package tfhoas
package semantics

import safecast.TypeSummoner
import trees._, TreeSerializable.ShowTree

class Serializer[Type[_]: ArrowType: TreeSerializable]
    extends tfhoas.Lambda[Type, ShowTree]
    with TypeSummoner[Type] {

  import Serializer.Constructors._

  def lam[A: Type, B: Type](f: (Int => Tree) => (Int => Tree)): Int => Tree =
    (i: Int) => {
      val x = "x" + i
      tr_lam(
        x,
        TreeSerializable[Type].show(Type[A]),
        f((_: Int) => tr_vr(x))(i + 1)
      )
    }

  def app[A: Type, B: Type](
      f: Int => Tree
  )(a: Int => Tree): Int => Tree =
    (i: Int) => tr_app(f(i), a(i))

  // Auxiliary

  def lam2[A: Type, B: Type, C: Type](
      f: (ShowTree[A], ShowTree[B]) => ShowTree[C]
  ): ShowTree[(A, B) => C] =
    (i: Int) => {
      val x = "x" + i
      val y = "x" + (i + 1)
      tr_lam2(
        x,
        TreeSerializable[Type].show(Type[A]),
        y,
        TreeSerializable[Type].show(Type[B]),
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

    def tr_lam2(name1: String, typ1: Tree, name2: String, typ2: Tree, body: Tree): Tree =
      Node("Lam2", List(Leaf(name1), typ1, Leaf(name2), typ2, body))

    def tr_curried(t: Tree): Tree =
      Node("Curry", List(t))

    def tr_tupled(t: Tree): Tree =
      Node("Tupled", List(t))
  }

  object Constructors extends Constructors
}
