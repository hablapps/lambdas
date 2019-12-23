package lambdas
package tfdb

import arithmetic.{ IntType, Num }
import IntType.Implicits._
import ArrowType.Implicits._

case class Examples[Type[_]: IntType: ArrowType, P[_, _]]()(implicit L: Lambda[Type, P]) {

  def ex1: P[(Num => Num, (Num, Unit)), Num] =
    L.app(L.vz[(Num, Unit), Num => Num])(L.vs(L.vz))
}
