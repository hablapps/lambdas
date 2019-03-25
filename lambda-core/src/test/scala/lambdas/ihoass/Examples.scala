// package lambdas
// package ihoass

// case class Examples[P[_]](p0: P[Int], p1: P[String]) {

//   trait Id {
//     type λ[L1 <: Lambda[P, Int]] = L1
//   }

//   type Id2[T, L1 <: Lambda[P, T]] = L1
//   type Id3[L1 <: Lambda[P, Int]]  = L1

//   val I =
//     Lam[P, Int, Int, Id#λ](
//       new Lambda.Nat[P, Int, Int, Id#λ] {
//         def apply[L1 <: ihoass.Lambda[P, Int]](a: L1): L1 = a
//       }
//     )

//   def A =
//     App[P, Int, Int, Lam[P, Int, Int, Id#λ], Var[P, Int]](I, Var(p0))

//   type Const0[T1, L1 <: Lambda[P, T1], T2, L2 <: Lambda[P, T2]] = L1
//   type TC[L1 <: Lambda[P, Int]]                                 = Lam[P, Int, Int, Const0[Int, L1, Int, ?]]

//   def K =
//     Lam[P, Int, Int => Int, TC] {
//       new Lambda.Nat[P, Int, Int => Int, TC] {
//         def apply[L1 <: ihoass.Lambda[P, Int]](l1: L1) =
//           Lam[P, Int, Int, Const0[Int, L1, Int, ?]] {
//             new Lambda.Nat[P, Int, Int, Const0[Int, L1, Int, ?]] {
//               def apply[L2 <: ihoass.Lambda[P, Int]](l2: L2): L1 =
//                 l1
//             }
//           }
//       }
//     }
// }
