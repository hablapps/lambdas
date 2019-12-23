package lambdas
package tfhoas
package semantics

object GenLambda {

  type GenArrow1[P[_], A, B] = P[A] => P[B]

  class IsoHOAS[Type[_]: ArrowType, P[_]](
      implicit
      IsoArrow1: IsoGen2[P, Function1, GenArrow1[P, ?, ?]]
  ) extends Lambda[Type, P] {

    def lam[A: Type, B: Type](f: P[A] => P[B]): P[A => B] =
      IsoArrow1[A, B].put(f)

    def app[A: Type, B: Type](f: P[A => B])(t1: P[A]): P[B] =
      IsoArrow1[A, B].get(f)(t1)
  }
}
