package lambdas
package tfdb

import arithmetic.{ IntType, Num }
import IntType.Implicits._
import ArrowType.Implicits._

case class Examples[Type[_]: IntType: ArrowType, P[_, _]]()(implicit L: Lambda[Type, P]) {

  def ex1: P[(Num, (Num => Num, Unit)), Num] =
    L.app(L.vs(L.vz): P[(Num, (Num => Num, Unit)), Num => Num])(
      L.vz: P[(Num, (Num => Num, Unit)), Num]
    )

  def ex2: P[Unit, (Num => Num) => Num => Num] =
    L.lam(
      L.lam(
        L.app(L.vs(L.vz): P[(Num, (Num => Num, Unit)), Num => Num])(
          L.vz: P[(Num, (Num => Num, Unit)), Num]
        )
      )
    )

  def ex2HOAS[L[_]](implicit H: tfhoas.Lambda[Type, L]): L[(Num => Num) => Num => Num] =
    H.lam((f: L[Num => Num]) => H.lam((n: L[Num]) => H.app(f)(n)))

}
