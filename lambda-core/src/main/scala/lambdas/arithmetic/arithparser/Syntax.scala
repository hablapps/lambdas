package lambdas
package trees
package arithparser

import lambdas.arithmetic.Num

trait Syntax {

  import scala.util.Try

  object IntT {
    def unapply(t: Tree): Option[Num] =
      t match {
        case Node("Int", List(Leaf(i))) =>
          Try(BigDecimal(i)).toOption
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

  object Max {
    def unapply(t: Tree): Option[(Tree, Tree)] =
      t match {
        case Node("Max", List(e1, e2)) =>
          Some((e1, e2))
        case _ =>
          None
      }
  }

  object Min {
    def unapply(t: Tree): Option[(Tree, Tree)] =
      t match {
        case Node("Min", List(e1, e2)) =>
          Some((e1, e2))
        case _ =>
          None
      }
  }

  object Abs {
    def unapply(t: Tree): Option[Tree] =
      t match {
        case Node("Abs", List(e1)) =>
          Some(e1)
        case _ =>
          None
      }
  }

  object Mult {
    def unapply(t: Tree): Option[(Tree, Tree)] =
      t match {
        case Node("Mult", List(e1, e2)) =>
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
