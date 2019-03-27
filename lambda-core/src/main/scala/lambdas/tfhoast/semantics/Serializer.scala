package lambdas
package tfhoast
package semantics

import trees._, Treeable.ShowTree

object Serializer {

  trait TypeSummoner[T[_]] {
    def Type[A](implicit T: T[A]) = T
  }

  implicit def _Lambda[Type[_]: ArrowType: Treeable] =
    new tfhoast.Lambda[Type, ShowTree] with TypeSummoner[Type] {
      import Constructors._

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
    }

  object Constructors {

    def tr_vr(name: String): Tree =
      Node("Var", List(Leaf(name)))

    def tr_lam(name: String, typ: Tree, body: Tree): Tree =
      Node("Lam", List(Leaf(name), typ, body))

    def tr_app(f: Tree, a: Tree): Tree =
      Node("App", List(f, a))
  }
}
