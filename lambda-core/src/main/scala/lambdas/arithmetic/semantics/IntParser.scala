package lambdas
package arithmetic
package semantics

import safecast._
import interpreters._
import trees._, syntax._

object IntParser {

  def parser[T[_]: IntType: Cast, F[_]](
      implicit
      A: Arithmetic[F],
      S: ForAll0[T, cats.Show]
  ) =
    OpenInterpreter[Tree, String, DynTerm[T, F]] { rec =>
      {
        case IntT(i) =>
          Right(DynTerm(IntType[T].tint, A.int(i)))

        case Add(e1, e2) =>
          for {
            dt1 <- rec(e1)
            dt2 <- rec(e2)
            _dt1 <- dt1
              .as(IntType[T].tint)
              .toEither(s"First operand of add, not an integer: ${S().show(dt1.typ)}")
            _dt2 <- dt2
              .as(IntType[T].tint)
              .toEither(s"Second operand of add, not an integer: ${S().show(dt2.typ)}")
          } yield DynTerm(IntType[T].tint, A.add(_dt1)(_dt2))
      }
    }(t => s"Not an int $t")

  def forall[P[_, _]: ForAll[?[_, _], Arithmetic], T[_]: IntType: Cast: ForAll0[?[_], cats.Show]] =
    new ForAll[P, λ[F[_] => OpenInterpreter[Tree, Either[String, DynTerm[T, F]]]]] {
      def apply[E] = parser[T, P[E, ?]]
    }
}
