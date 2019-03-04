package lambdas
package deserialization

import taglessfinal.debruijn._
import org.scalatest._

class DynTermSpec extends Matchers{

  val d0 = DynTerm(tint[TQ], int[Function1, Unit](1))

  def d1[P[_, _]: Lambda]: DynTerm[P, Unit] =
    DynTerm(tint[TQ], 1+2)

  def d2[P[_, _]: Lambda]: DynTerm[P, Unit] =
    DynTerm(tint[TQ] -> tint[TQ], lam(vz[P, Unit, Int]+1))

  def d3[P[_, _]: Lambda]: DynTerm[P, Unit] =
    // DynTerm(tint[TQ], lam(add(vz, int(1))).apply(1))
    DynTerm(tint[TQ], lam(vz[P, Unit, Int]+1).apply(1))
}
