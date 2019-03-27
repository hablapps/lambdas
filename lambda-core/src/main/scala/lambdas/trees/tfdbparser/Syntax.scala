package lambdas
package trees
package tfdbparser

trait Syntax {

  object Var {
    def unapply(t: Tree): Option[String] =
      t match {
        case Node("Var", List(Leaf(name))) =>
          Some(name)
        case _ =>
          None
      }
  }

  object Lam {
    def unapply(t: Tree): Option[(String, Tree, Tree)] =
      t match {
        case Node("Lam", List(Leaf(name), typ, body)) =>
          Some((name, typ, body))
        case _ =>
          None
      }
  }

  object App {
    def unapply(t: Tree): Option[(Tree, Tree)] =
      t match {
        case Node("App", List(ft, at)) =>
          Some((ft, at))
        case _ =>
          None
      }
  }

  object TArr {
    def unapply(t: Tree): Option[(Tree, Tree)] =
      t match {
        case Node("TArr", List(t1, t2)) =>
          Some((t1, t2))
        case _ =>
          None
      }
  }
}
