package lambdas
package records
package parser

import trees._

object Extractors {

  object TreeRecord {
    def unapply(t: Tree): Option[List[(String, Tree)]] =
      t match {
        case Node("Record", fields) =>
          fields.foldLeft(Option(List[(String, Tree)]())) {
            case (acc, Node("Field", List(Leaf(key), value))) =>
              acc.map((key, value) :: _)
            case _ => None
          }
        case _ =>
          None
      }
  }

  object TreeSelect {
    def unapply(t: Tree): Option[(Tree, String)] =
      t match {
        case Node("Select", List(r1, Leaf(f1))) =>
          Some((r1, f1))
        case _ =>
          None
      }
  }

  object TreeRecordType {
    def unapply(t: Tree): Option[List[(String, Tree)]] =
      t match {
        case Node("RecordType", fields) =>
          fields.foldLeft(Option(List[(String, Tree)]())) {
            case (acc, Node("FieldType", List(Leaf(key), typ))) =>
              acc.map((key, typ) :: _)
            case _ => None
          }
        case _ =>
          None
      }
  }
}
