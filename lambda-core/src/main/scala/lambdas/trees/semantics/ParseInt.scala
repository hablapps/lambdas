package lambdas
package trees
package semantics

import syntax._
import safecast._
import interpreters._
import arithmetic._

case class ParseInt[F[_]]()(implicit A: Arithmetic[F])
    extends Interpreter[Tree, Either[String, DynTerm[F]]] {

  def apply(tree: Tree) = tree match {
    case IntT(i) =>
      Right(DynTerm(tint[TypeTerm], A.int(i)))

    case Add(e1, e2) =>
      for {
        dt1 <- apply(e1)
        dt2 <- apply(e2)
        _dt1 <- dt1
          .as(tint[TypeTerm])
          .toEither(s"First operand of add, not an integer: ${dt1.typ}")
        _dt2 <- dt2.asInt.toEither(s"Second operand of add, not an integer: ${dt2.typ}")
      } yield DynTerm(tint[TypeTerm], A.add(_dt1)(_dt2))
  }
}
