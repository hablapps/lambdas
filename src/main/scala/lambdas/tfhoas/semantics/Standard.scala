package lambdas
package tfhoas
package semantics

object Std extends Lambda[cats.Id] {

  def int(i: Int): Int = i

  def add(i1: Int)(i2: Int): Int = i1 + i2

  def lam[T1, T2](f: T1 => T2): T1 => T2 = f

  def app[T1, T2](f: T1 => T2)(t1: T1): T2 = f(t1)
}
