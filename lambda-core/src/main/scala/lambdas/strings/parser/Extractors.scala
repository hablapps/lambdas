package lambdas
package strings
package parser

import trees._

object Extractors {

  object AString {
    def unapply(t: Tree): Option[String] =
      t match {
        case Node("String", List(Leaf(s))) =>
          Some(s)
        case _ =>
          None
      }
  }

  object Length {
    def unapply(t: Tree): Option[Tree] =
      t match {
        case Node("Length", List(s)) =>
          Some(s)
        case _ =>
          None
      }
  }

  object TString {
    def unapply(t: Tree): Boolean =
      t match {
        case Leaf("TString") =>
          true
        case _ =>
          false
      }
  }
}
