// package lambdas
// package ihoass
// package semantics

// import tfhoas.{ Lambda => FLambda }

// case class FinallyLambda[P[_]]()(implicit F: FLambda[P]) {

//   def apply[T](l: Lambda[P, T]): P[T] = l match {
//     case Var(v)    => v
//     case Lam(f)    => F.lam(pt => apply(f(Var(pt))))
//     case App(f, a) => F.app(apply(f))(apply(a))
//   }
// }
