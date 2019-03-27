package lambdas
package initial
package hoas

sealed abstract class Lambda[Type[_]: ArrowType, P[_], T]
case class Var[P[_], Type[_]: ArrowType, T](p: P[T], T: Type[T]) extends Lambda[Type, P, T]
case class Lam[P[_], Type[_]: ArrowType, T1, T2](
    f: Lambda[Type, P, T1] => Lambda[Type, P, T2],
    T1: Type[T1],
    T2: Type[T2]
) extends Lambda[Type, P, T1 => T2]
case class App[P[_], Type[_]: ArrowType, T1, T2](
    f: Lambda[Type, P, T1 => T2],
    t1: Lambda[Type, P, T1],
    T1: Type[T1],
    T2: Type[T2]
) extends Lambda[Type, P, T2]
