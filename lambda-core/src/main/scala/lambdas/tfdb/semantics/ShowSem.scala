package lambdas
package tfdb
package semantics

object ShowSem extends Lambda[ShowB] {

  def int[E](i: Int): Int => String =
    _ => i.toString

  def add[E](i1: Int => String, i2: Int => String): Int => String =
    i => "(" + i1(i) + "+" + i2(i) + ")"

  def vz[E, T]: Int => String =
    i => {
      val n = if (i - 1 < 0) "y" else "x"
      val j = (i - 1).abs
      s"$n$j"
    }

  def vs[E, T, T1](a: Int => String): Int => String =
    i => a(i - 1)

  def lam[E, T1, T2](t: Int => String): Int => String =
    i => s"(λx$i.${t(i + 1)})"

  def app[E, T1, T2](f: Int => String)(t1: Int => String): Int => String =
    i => "(" + f(i) + " " + t1(i) + ")"
}
