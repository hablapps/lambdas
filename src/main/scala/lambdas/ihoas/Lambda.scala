package lambdas
package initial
package hoas

sealed abstract class Lambda[P[_], T]
case class IntL[P[_]](i: Int) extends Lambda[P, Int]
case class Add[P[_]](i1: Lambda[P, Int], i2: Lambda[P, Int]) extends Lambda[P, Int]
case class Var[P[_], T](p: P[T]) extends Lambda[P, T]
case class Lam[P[_], T1, T2](f: Lambda[P, T1] => Lambda[P, T2]) extends Lambda[P, T1 => T2]
case class App[P[_], T1, T2](f: Lambda[P, T1 => T2], t1: Lambda[P, T1]) extends Lambda[P, T2]
