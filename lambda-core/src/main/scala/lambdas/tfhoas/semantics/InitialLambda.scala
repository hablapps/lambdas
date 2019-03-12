package lambdas
package tfhoas
package semantics

import initial.hoas.{ Lambda => ILambda, _ }

class InitialLambda[P[_]] extends Lambda[({ type λ[T] = ILambda[P, T] })#λ] {
  // Lambda[ILambda[P, ?]] { //doesn't work?!

  def int(i: Int): ILambda[P, Int] =
    IntL(i)

  def add(i1: ILambda[P, Int])(i2: ILambda[P, Int]): ILambda[P, Int] =
    Add(i1, i2)

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
