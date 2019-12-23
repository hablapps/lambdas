package lambdas
package tfdb
package semantics

import trees._, TreeSerializable.ShowTreeB

class LambdaSerializer[Type[_]: ArrowType: TreeSerializable] extends Lambda[Type, ShowTreeB] {

  import tfhoas.semantics.Serializer.Constructors._

  def vz[E, T: Type]: Int => Tree =
    i => {
      val v = if (i - 1 < 0) "y" else "x"
      val j = (i - 1).abs
      tr_vr(s"$v$j")
    }

  def vs[E, T: Type, T1: Type](a: Int => Tree): Int => Tree =
    i => a(i - 1)

  def lam[E, T1, T2](t: Int => Tree)(implicit T1: Type[T1], T2: Type[T2]): Int => Tree =
    (i: Int) => {
      val x = "x" + i
      tr_lam(
        x,
        TreeSerializable[Type].show(T1),
        t(i + 1)
      )
    }

  def app[E, T1: Type, T2: Type](f: Int => Tree)(t1: Int => Tree): Int => Tree =
    (i: Int) => tr_app(f(i), t1(i))
}
