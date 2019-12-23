package lambdas
package records
package semantics

import shapeless.{ HList, Witness }
import cats.data.Const

object ShowRecords extends Records[Const[String, ?]] {

  def record[L <: HList](fields: Fields[Const[String, ?], L]): Const[String, Record[L]] = {
    def show: Fields[Const[String, ?], _] => List[String] = {
      case _: NilFields[Const[String, ?]] =>
        List()
      case hc: HConsFields[Const[String, ?], _, _, _] =>
        (hc.W.value + "=" + hc.head.getConst) :: show(hc.tail)
    }
    Const(s"{${show(fields).mkString(", ")}}")
  }

  def field[L <: HList, K](record: Const[String, Record[L]], key: Witness.Aux[K])(
      implicit
      S: Fields.Selector[Const[String, ?], L, K]
  ): Const[String, S.Out] =
    Const(s"${record.getConst}.${key.value}")
}
