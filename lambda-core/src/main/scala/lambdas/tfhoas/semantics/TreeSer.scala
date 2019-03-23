package lambdas
package tfhoas

import cats.data.Const
import trees._

object TreeSer extends Lambda[({ type λ[T] = Const[Tree, T] })#λ] {

  def tuple[A, B](a: Const[Tree, A], b: Const[Tree, B]): Const[Tree, (A, B)] =
    ???

  def fst[A, B](t: Const[Tree, (A, B)]): Const[Tree, A] =
    ???

  def snd[A, B](t: Const[Tree, (A, B)]): Const[Tree, B] =
    ???

  def tupled[A, B, C](f: Const[Tree, (A, B) => C]): Const[Tree, ((A, B)) => C] =
    ???

  def curried[A, B, C](f: Const[Tree, (A, B) => C]): Const[Tree, A => B => C] =
    ???

  def lam[T1, T2](f: Const[Tree, T1] => Const[Tree, T2]): Const[Tree, T1 => T2] =
    ???

  def lam2[A, B, C](
      f: (Const[Tree, A], Const[Tree, B]) => Const[Tree, C]
  ): Const[Tree, (A, B) => C] =
    ???

  def app[T1, T2](f: Const[Tree, T1 => T2])(t1: Const[Tree, T1]): Const[Tree, T2] =
    ???
}
