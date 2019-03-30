package lambdas
package trees
package arithparser

import safecast._
import interpreters._
import arithmetic._

case class IntParser[T[_]: IntType: Cast, F[_]]()(
    implicit
    A: Arithmetic[F],
    S: Forall0[T, cats.Show]
) extends OpenInterpreter[Tree, Either[String, DynTerm[T, F]]] {

  def apply(rec: => Interpreter[Tree, Either[String, DynTerm[T, F]]]) = {
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

    case t => Left(s"Not an int $t")
  }
}

object IntParser {

  import trees.tfdbparser.ArrowParser.OpenInterpreterEither

  def forall[P[_, _]: Forall[?[_, _], Arithmetic], T[_]: IntType: Cast: Forall0[?[_], cats.Show]] =
    new Forall[P, OpenInterpreterEither[T, ?[_]]] {
      def apply[E] = IntParser[T, P[E, ?]]()
    }
}
