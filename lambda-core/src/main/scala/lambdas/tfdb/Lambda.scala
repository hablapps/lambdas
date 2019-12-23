package lambdas
package tfdb

import ArrowType.Implicits._

abstract class Lambda[Type[_]: ArrowType, P[E, T]] extends Serializable {
  def vz[E, T: Type]: P[(T, E), T]
  def vs[E, T: Type, T1: Type](a: P[E, T]): P[(T1, E), T]
  def lam[E, T1: Type, T2: Type](t: P[(T1, E), T2]): P[E, T1 => T2]
  def app[E, T1: Type, T2: Type](f: P[E, T1 => T2])(t1: P[E, T1]): P[E, T2]

  def lam2[E, T1: Type, T2: Type, T3: Type](t: P[(T2, (T1, E)), T3]): P[E, T1 => T2 => T3] =
    lam(lam(t))
}

object Lambda {
  def apply[Type[_]: ArrowType, P[E, T]](implicit L: Lambda[Type, P]) = L

  implicit def ShowSem[Type[_]: ArrowType]: Lambda[Type, ShowB] = new semantics.ShowSem[Type]
  implicit def ShowTreeBSem[Type[_]: ArrowType: trees.TreeSerializable] =
    new semantics.LambdaSerializer[Type]
  implicit def StdSem[Type[_]: ArrowType]: Lambda[Type, Function1] = new semantics.Standard[Type]
  implicit def TermSem[Type[_]: ArrowType]: Lambda[Type, semantics.Term[Type, ?, ?]] =
    new semantics.Term.TermLambda[Type]
  implicit def TupledSem[Type[_]: ArrowType, P1[_, _]: Lambda[Type, ?[_, _]], P2[_, _]: Lambda[
    Type,
    ?[_, _]
  ]] = new semantics.TupledInstance[Type, P1, P2]
}
