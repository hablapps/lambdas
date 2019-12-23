package lambdas
package tfdb
package semantics

class ShowSem[Type[_]: ArrowType] extends Lambda[Type, ShowB] {

  def vz[E, T: Type]: Int => String =
    i => {
      val n = if (i - 1 < 0) "y" else "x"
      val j = (i - 1).abs
      s"$n$j"
    }

  def vs[E, T: Type, T1: Type](a: Int => String): Int => String =
    i => a(i - 1)

  def lam[E, T1: Type, T2: Type](t: Int => String): Int => String =
    i => s"(λx$i.${t(i + 1)})"

  def app[E, T1: Type, T2: Type](f: Int => String)(t1: Int => String): Int => String =
    i => "(" + f(i) + " " + t1(i) + ")"
}
