package lambdas
package trees
package semantics

import syntax._
import safecast._
import interpreters._
import arithmetic._

object IntTermParser {

  def parser[F[_]](implicit A: Arithmetic[F]) =
    OpenInterpreter[Tree, String, DynTerm[F]] { rec =>
      {
        case IntT(i) =>
          Right(DynTerm(tint[TypeTerm], A.int(i)))

        case Add(e1, e2) =>
          for {
            dt1 <- rec(e1)
            dt2 <- rec(e2)
            _dt1 <- dt1
              .as(tint[TypeTerm])
              .toEither(s"First operand of add, not an integer: ${dt1.typ[ShowP]}")
            _dt2 <- dt2.asInt.toEither(s"Second operand of add, not an integer: ${dt2.typ[ShowP]}")
          } yield DynTerm(tint[TypeTerm], A.add(_dt1)(_dt2))
      }
    }(t => s"Not an int $t")
}
