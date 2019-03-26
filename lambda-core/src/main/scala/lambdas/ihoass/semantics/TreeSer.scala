package lambdas
package ihoass
package semantics

import trees._

trait TreeSer[A, T <: Lambda[TreeSer.Show, A, T]] {
  def apply(l: Lambda[TreeSer.Show, A, T]): TreeSer.Show[A]
}

object TreeSer {

  type Show[T] = Int => Tree

  def apply[A, T <: Lambda[Show, A, T]](l: Lambda[Show, A, T])(implicit T: TreeSer[A, T]): Show[T] =
    T(l)

  import Constructors._

  implicit def _Var[A, T] = new TreeSer[A, Var[Show, A]] {
    def apply(l: Lambda[Show, A, Var[Show, A]]): Int => Tree =
      l match {
        case Var(p) => p
      }
  }

  // implicit def _Lam[Type[_], T1, T2, L2[L1 <: Lambda[Show, T1]] <: Lambda[Show, T2]](
  //     implicit
  //     Treeable: Treeable[Type],
  //     TypeT1: Type[T1],
  //     Ser2: TreeSer[T2, L2[Var[Show, T1]]]
  // ) =
  //   new TreeSer[T1 => T2, Lam[Show, T1, T2, L2]] {
  //     def apply(l: Lam[Show, T1, T2, L2]): Int => Tree =
  //       (i: Int) => {
  //         val x = "x" + i
  //         tr_lam(
  //           x,
  //           Treeable.show(TypeT1),
  //           Ser2(l.f(Var[Show, T1]((_: Int) => tr_vr(x))))(i + 1)
  //         )
  //       }
  //   }

  // implicit def _App[T1, T2, L1 <: Lambda[Show, T1 => T2], L2 <: Lambda[Show, T1]](
  //     implicit
  //     TS_f: TreeSer[T1 => T2, L1],
  //     TS_1: TreeSer[T1, L2]
  // ) =
  //   new TreeSer[T2, App[Show, T1, T2, L1, L2]] {
  //     def apply(l: App[Show, T1, T2, L1, L2]): Int => Tree =
  //       (i: Int) => tr_app(TS_f(l.f)(i), TS_1(l.t1)(i))
  //   }

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
