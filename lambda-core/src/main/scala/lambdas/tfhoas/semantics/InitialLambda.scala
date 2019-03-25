package lambdas
package tfhoas
package semantics

import initial.hoas.{ Lambda => ILambda, _ }

class InitialLambda[P[_]] extends tfhoas.Lambda[ILambda[P, ?]] {

  def tuple[A, B](a: ILambda[P, A], b: ILambda[P, B]): ILambda[P, (A, B)] = ???

  def fst[A, B](t: ILambda[P, (A, B)]): ILambda[P, A] = ???

  def snd[A, B](t: ILambda[P, (A, B)]): ILambda[P, B] = ???

  def tupled[A, B, C](f: ILambda[P, (A, B) => C]): ILambda[P, ((A, B)) => C] = ???

  def curried[A, B, C](f: ILambda[P, (A, B) => C]): ILambda[P, A => B => C] = ???

  def lam2[A, B, C](f: (ILambda[P, A], ILambda[P, B]) => ILambda[P, C]): ILambda[P, (A, B) => C] =
    ???

  def lam[T1, T2](f: ILambda[P, T1] => ILambda[P, T2]): ILambda[P, T1 => T2] =
    Lam(f)

  def app[T1, T2](f: ILambda[P, T1 => T2])(t1: ILambda[P, T1]): ILambda[P, T2] =
    App(f, t1)
}
