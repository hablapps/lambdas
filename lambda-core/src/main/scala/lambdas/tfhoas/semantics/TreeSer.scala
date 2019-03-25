// package lambdas
// package tfhoas

// import cats.data.Const
// import trees._

// object TreeSer {
//   import Constructors._

//   trait Run[A, S <: TreeSer[A]] {
//     def apply(s: S): Int => Tree
//   }

//   object Run {

//     implicit def _Unk[A] = new Run[A, Unk[A]] {
//       def apply(s: Unk[A]) = s.t
//     }

//     implicit def _LamVar[A, B, S <: TreeSer[B], Type[_]](
//         implicit
//         Treeable: Treeable[Type],
//         tA: Type[A],
//         R: Run[B, S]
//     ) = new Run[A => B, LamVar[A, B, S]] {
//       def apply(s: LamVar[A, B, S]) = (i: Int) => {
//         val x = "x" + i
//         tr_lam(
//           x,
//           Treeable.show(tA),
//           R(s.body(tr_vr(x)))(i + 1)
//         )
//       }
//     }
//   }

//   implicit object _Lambda extends tfhoas.Lambda[TreeSer] {
//     import Constructors._

//     def lam[T1, T2](f: TreeSer[T1] => TreeSer[T2]): TreeSer[T1 => T2] =
//       LamVar()

//     def app[A: Type, B: Type](
//         f: ShowTree[Type, A => B]
//     )(a: ShowTree[Type, A]): ShowTree[Type, B] =
//       ShowTree((i: Int) => (tr_app(f(i)._1, a(i)._1), Type[B]))

//     def app[T1, T2](f: TreeSer[T1 => T2])(t1: TreeSer[T1]): TreeSer[T2] =
//       ???

//     // def tuple[A, B](a: TreeSer[A], b: TreeSer[B]): TreeSer[(A, B)] =
//     //   ???

//     // def fst[A, B](t: TreeSer[(A, B)]): TreeSer[A] =
//     //   ???

//     // def snd[A, B](t: TreeSer[(A, B)]): TreeSer[B] =
//     //   ???

//     // def tupled[A, B, C](f: TreeSer[(A, B) => C]): TreeSer[((A, B)) => C] =
//     //   ???

//     // def curried[A, B, C](f: TreeSer[(A, B) => C]): TreeSer[A => B => C] =
//     //   ???

//     // def lam2[A, B, C](
//     //     f: (TreeSer[A], TreeSer[B]) => TreeSer[C]
//     // ): TreeSer[(A, B) => C] =
//     //   ???
//   }

//   object Constructors {

//     def tr_vr(name: String): Tree =
//       Node("Var", List(Leaf(name)))

//     def tr_lam(name: String, typ: Tree, body: Tree): Tree =
//       Node("Lam", List(Leaf(name), typ, body))

//     def tr_app(f: Tree, a: Tree): Tree =
//       Node("App", List(f, a))

//     def tr_tArr(t1: Tree, t2: Tree): Tree =
//       Node("TArr", List(t1, t2))
//   }
// }
