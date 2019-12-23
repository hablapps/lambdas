package lambdas.tfhoas.productsparser

import lambdas.trees.{ Node, Tree }

trait Syntax {

  object Tuple {
    def unapply(t: Tree): Option[(Tree, Tree)] =
      t match {
        case Node("Tuple2", List(e1, e2)) =>
          Some((e1, e2))
        case _ =>
          None
      }
  }
}
