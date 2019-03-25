package lambdas
package tfhoast
package semantics

import trees._
import tfdb.ArrowType

abstract class ShowTree[Type[_], A] extends (Int => (Tree, Type[A]))

object ShowTree {

  def apply[Type[_], A](f: Int => (Tree, Type[A])) = new ShowTree[Type, A] {
    def apply(i: Int) = f(i)
  }

  trait TypeSummoner[T[_]] {
    def Type[A](implicit T: T[A]) = T
  }

  implicit def _Lambda[Type[_]: ArrowType: Treeable] =
    new tfhoast.Lambda[Type, ShowTree[Type, ?]] with TypeSummoner[Type] {
      import Constructors._

      def lam[A: Type, B: Type](f: ShowTree[Type, A] => ShowTree[Type, B]): ShowTree[Type, A => B] =
        ShowTree((i: Int) => {
          val x = "x" + i
          (
            tr_lam(
              x,
              Treeable[Type].show(Type[A]),
              f((_: Int) => (tr_vr(x), Type[A]))(i + 1)._1
            ),
            ArrowType[Type].tarrow(Type[A], Type[B])
          )
        })

      def app[A: Type, B: Type](
          f: ShowTree[Type, A => B]
      )(a: ShowTree[Type, A]): ShowTree[Type, B] =
        ShowTree((i: Int) => (tr_app(f(i)._1, a(i)._1), Type[B]))
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
