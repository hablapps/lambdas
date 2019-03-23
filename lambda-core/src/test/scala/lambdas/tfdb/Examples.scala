package lambdas
package tfdb

case class Examples[P[_, _]]()(implicit L: Lambda[P]) {

  def ex1: P[(Int => Int, (Int, Unit)), Int] =
    L.app(L.vz[(Int, Unit), Int => Int])(L.vs(L.vz))
}
