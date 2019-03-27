// package lambdas
// package tfhoas
// package semantics

// import idb.{ Lambda => ILambda, _ }

// class InitialDB[E] extends tfhoas.Lambda[ILambda[E, ?]] {

//   def lam[T1, T2](f: ILambda[E, T1] => ILambda[E, T2]): ILambda[E, T1 => T2] =
//     Lam[E, T1, T2, ILambda[(T1, E), T2]](f(Vz[E, T1]))

//   def app[T1, T2](f: ILambda[E, T1 => T2])(t1: ILambda[E, T1]): ILambda[E, T2] =
//     App[E, T1, T2, ILambda[E, T1 => T2], ILambda[E, T1]](f, t1)

//   // Products

//   def tuple[A, B](a: ILambda[E, A], b: ILambda[E, B]): ILambda[E, (A, B)] =
//     ???

//   def fst[A, B](t: ILambda[E, (A, B)]): ILambda[E, A] =
//     ???

//   def snd[A, B](t: ILambda[E, (A, B)]): ILambda[E, B] =
//     ???

//   // Auxiliary

//   def lam2[A, B, C](f: (ILambda[E, A], ILambda[E, B]) => ILambda[E, C]): ILambda[E, (A, B) => C] =
//     ???

//   def curried[A, B, C](f: ILambda[E, (A, B) => C]): ILambda[E, A => B => C] =
//     ???

//   def tupled[A, B, C](f: ILambda[E, (A, B) => C]): ILambda[E, ((A, B)) => C] =
//     ???
// }
