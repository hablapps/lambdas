package lambdas
package idb
package semantics

import safecast.Typeable

object Metamodel {

  sealed abstract class IntStringType[_]
  case object IntType    extends IntStringType[Int]
  case object StringType extends IntStringType[String]

  object IntStringType {

    import trees._

    implicit val _TreeSerializable = new TreeSerializable[IntStringType] {
      def show[T](T: IntStringType[T]): Tree = T match {
        case IntType    => Leaf("TInt")
        case StringType => Leaf("TString")
      }
    }
  }

  implicit val TInt    = Typeable[Int, IntStringType](IntType)
  implicit val TString = Typeable[String, IntStringType](StringType)
}
