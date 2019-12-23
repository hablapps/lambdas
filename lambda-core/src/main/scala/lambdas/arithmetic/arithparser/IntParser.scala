package lambdas
package trees
package arithparser

import lambdas.arithmetic._
import lambdas.interpreters._
import lambdas.safecast._
import cats.syntax.either._

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
          .toEither(s"\nFirst operand of add, not an integer: ${S().show(dt1.typ)}")
        _dt2 <- dt2
          .as(IntType[T].tint)
          .toEither(s"\nSecond operand of add, not an integer: ${S().show(dt2.typ)}")
      } yield DynTerm(IntType[T].tint, A.add(_dt1)(_dt2))

    case Mult(e1, e2) =>
      for {
        dt1 <- rec(e1)
        dt2 <- rec(e2)
        _dt1 <- dt1
          .as(IntType[T].tint)
          .toEither(s"\nFirst operand of mult, not an integer: ${S().show(dt1.typ)}")
        _dt2 <- dt2
          .as(IntType[T].tint)
          .toEither(s"\nSecond operand of mult, not an integer: ${S().show(dt2.typ)}")
      } yield DynTerm(IntType[T].tint, A.mult(_dt1)(_dt2))
    case Max(e1, e2) =>
      for {
        dt1 <- rec(e1)
        dt2 <- rec(e2)
        _dt1 <- dt1
          .as(IntType[T].tint)
          .toEither(s"\nFirst operand of max, not an integer: ${S().show(dt1.typ)}")
        _dt2 <- dt2
          .as(IntType[T].tint)
          .toEither(s"\nSecond operand of max, not an integer: ${S().show(dt2.typ)}")
      } yield DynTerm(IntType[T].tint, A.max(_dt1)(_dt2))
    case Min(e1, e2) =>
      for {
        dt1 <- rec(e1)
        dt2 <- rec(e2)
        _dt1 <- dt1
          .as(IntType[T].tint)
          .toEither(s"\nFirst operand of min, not an integer: ${S().show(dt1.typ)}")
        _dt2 <- dt2
          .as(IntType[T].tint)
          .toEither(s"\nSecond operand of min, not an integer: ${S().show(dt2.typ)}")
      } yield DynTerm(IntType[T].tint, A.min(_dt1)(_dt2))
    case Abs(e) =>
      for {
        dt <- rec(e)
        _dt <- dt
          .as(IntType[T].tint)
          .toEither(s"\nOperand of abs, not an integer: ${S().show(dt.typ)}")
      } yield DynTerm(IntType[T].tint, A.abs(_dt))
//    case t =>
//      Left(s"\nNot an int term $t")
    case _ => Left("")
  }
}

object IntParser {

  import trees.tfdbparser.ArrowParser.OpenInterpreterEither

  def forall[P[_, _]: Forall[?[_, _], Arithmetic], T[_]: IntType: Cast: Forall0[
    ?[_],
    cats.Show
  ]] =
    new Forall[P, OpenInterpreterEither[T, ?[_]]] {
      def apply[E] = IntParser[T, P[E, ?]]()
    }
}
