package lambdas
package tfdb
package semantics

sealed abstract class Derivation[E, A]

case class VarZ[A, E]() extends Derivation[(A, E), A]

case class VarS[A, B, E](
    d: Derivation[E, A]
) extends Derivation[(B, E), A]

case class ImplI[A, B, E](
    d1: Derivation[(A, E), B]
) extends Derivation[E, A => B]

case class ImplE[A, B, E](
    d1: Derivation[E, A => B],
    d2: Derivation[E, A]
) extends Derivation[E, B]

object Derivation {

  class DerivationDB[Type[_]: ArrowType] extends Lambda[Type, Derivation] {
    def vz[E, T: Type]: Derivation[(T, E), T] =
      VarZ()

    def vs[E, T: Type, T1: Type](a: Derivation[E, T]): Derivation[(T1, E), T] =
      VarS(a)

    def lam[E, T1: Type, T2: Type](t: Derivation[(T1, E), T2]): Derivation[E, T1 => T2] =
      ImplI(t)

    def app[E, T1: Type, T2: Type](f: Derivation[E, T1 => T2])(
        t1: Derivation[E, T1]
    ): Derivation[E, T2] =
      ImplE(f, t1)
  }
}
/*
case class NotE[A, B, E](
    d1: Derivation[E, A],
    d2: Derivation[E, Not[A]]
) extends Derivation[E, B]

case class AndI[A, B, E](
    d1: Derivation[E, A],
    d2: Derivation[E, B]
) extends Derivation[E, A And B]

case class AndE1[A, B, E](
    d1: Derivation[E, A And B]
) extends Derivation[E, A]

case class AndE2[A, B, E](
    d1: Derivation[E, A And B]
) extends Derivation[E, B]

case class OrI1[A, B, E](
    d1: Derivation[E, A]
) extends Derivation[E, A Or B]

case class OrI2[A, B, E](
    d1: Derivation[E, B]
) extends Derivation[E, A Or B]

case class OrE[A, B, C, E](
    d1: Derivation[E, A Or B],
    d2: Derivation[E, A => C],
    d3: Derivation[E, B => C]
) extends Derivation[E, C]
 */
