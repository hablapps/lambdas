package lambdas
package records
package semantics

import trees._, TreeSerializable.ShowTree
import shapeless.{ HList, Witness }

object Serializer extends Records[ShowTree] {

  import Constructors._

  def record[L <: HList](fields: Fields[ShowTree, L]): ShowTree[Record[L]] = i => {
    def show: Fields[ShowTree, _] => List[(String, Tree)] = {
      case _: NilFields[ShowTree] =>
        List()
      case hc: HConsFields[ShowTree, _, _, _] =>
        (hc.W.value.toString, hc.head(i)) :: show(hc.tail)
    }
    tr_record(show(fields))
  }

  def field[L <: HList, K](record: ShowTree[Record[L]], key: Witness.Aux[K])(
      implicit
      S: Fields.Selector[ShowTree, L, K]
  ): ShowTree[S.Out] =
    i => tr_field(record(i), key.value.toString)

  object Constructors {

    def tr_record(fields: List[(String, Tree)]): Tree =
      Node("Record", fields.map {
        case (key, value) =>
          Node("Field", List(Leaf(key), value))
      })

    def tr_field(record: Tree, field: String): Tree =
      Node("Select", List(record, Leaf(field)))
  }
}
