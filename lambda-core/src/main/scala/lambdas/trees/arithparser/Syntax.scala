package lambdas
package trees
package arithparser

trait Syntax {

  import scala.util.Try

  object IntT {
    def unapply(t: Tree): Option[Int] =
      t match {
        case Node("Int", List(Leaf(i))) =>
          Try(Integer.parseInt(i)).toOption
        case _ =>
          None
      }
  }

  object Add {
    def unapply(t: Tree): Option[(Tree, Tree)] =
      t match {
        case Node("Add", List(e1, e2)) =>
          Some((e1, e2))
        case _ =>
          None
      }
  }

  object TInt {
    def unapply(t: Tree): Boolean =
      t match {
        case Leaf("TInt") =>
          true
        case _ =>
          false
      }
  }
}
