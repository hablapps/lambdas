package lambdas
package tfhoast

import tfdb.ArrowType

abstract class Lambda[Type[_]: ArrowType, P[_]] {

  def lam[A: Type, B: Type](f: P[A] => P[B]): P[A => B]

  def app[A: Type, B: Type](f: P[A => B])(t1: P[A]): P[B]
}

object Lambda {

  def apply[Type[_], P[_]](implicit L: Lambda[Type, P]) = L
}
