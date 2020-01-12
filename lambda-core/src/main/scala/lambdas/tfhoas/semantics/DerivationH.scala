package lambdas
package tfhoas
package semantics

sealed abstract class Derivation[A]

case class Var[A](x: Int) extends Derivation[A]

case class ImplI[A, B](
    f: Derivation[A] => Derivation[B]
) extends Derivation[A => B]

case class ImplE[A, B](
    d1: Derivation[A => B],
    d2: Derivation[A]
) extends Derivation[B]

case class AndI[A, B, E](
    d1: Derivation[A],
    d2: Derivation[B]
) extends Derivation[(A, B)]

case class AndE1[A, B, E](
    d1: Derivation[(A, B)]
) extends Derivation[A]

case class AndE2[A, B, E](
    d1: Derivation[(A, B)]
) extends Derivation[B]

object Derivation {

  implicit def DerivationLambda[Type[_]: ArrowType] =
    new Lambda[Type, Derivation] with Products[Derivation] {
      def lam[T1: Type, T2: Type](f: Derivation[T1] => Derivation[T2]): Derivation[T1 => T2] =
        ImplI(f)

      def app[T1: Type, T2: Type](f: Derivation[T1 => T2])(
          t1: Derivation[T1]
      ): Derivation[T2] =
        ImplE(f, t1)

      def tuple[A, B](a: Derivation[A], b: Derivation[B]): Derivation[(A, B)] =
        AndI(a, b)

      def fst[A, B](t: Derivation[(A, B)]): Derivation[A] =
        AndE1(t)

      def snd[A, B](t: Derivation[(A, B)]): Derivation[B] =
        AndE2(t)
    }

  def show[A](d: Derivation[A], i: Int = 0): String = d match {
    case Var(x)       => s"x$x"
    case ImplI(f)     => s"ImplI(x$i, " + show(f(Var(i)), i + 1) + ")"
    case ImplE(f, a)  => s"ImplE(${show(f, i)}, ${show(a, i)})"
    case AndI(d1, d2) => s"AndI(${show(d1, i)}, ${show(d2, i)})"
    case AndE1(d)     => s"AndE1(${show(d, i)})"
    case AndE2(d)     => s"AndE2(${show(d, i)})"
  }
}
/*
case class NotE[A, B, E](
    d1: Derivation[A],
    d2: Derivation[Not[A]]
) extends Derivation[B]


case class OrI1[A, B, E](
    d1: Derivation[A]
) extends Derivation[A Or B]

case class OrI2[A, B, E](
    d1: Derivation[B]
) extends Derivation[A Or B]

case class OrE[A, B, C, E](
    d1: Derivation[A Or B],
    d2: Derivation[A => C],
    d3: Derivation[B => C]
) extends Derivation[C]
 */
