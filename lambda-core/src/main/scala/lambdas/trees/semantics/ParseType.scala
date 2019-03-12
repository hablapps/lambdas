package lambdas
package trees
package semantics

import syntax._
import safecast._

object ParseType extends Semantics[Either[String, ATypeTerm]] {

  def apply(t: Tree): Either[String, ATypeTerm] = t match {

    case TInt() =>
      Right(ATypeTerm(tint[TypeTerm]))

    case TArr(t1, t2) =>
      for {
        t1t <- apply(t1)
        t2t <- apply(t2)
      } yield ATypeTerm(t1t.typ -> t2t.typ)

    case _ =>
      Left(s"Not a type: $t")
  }
}
