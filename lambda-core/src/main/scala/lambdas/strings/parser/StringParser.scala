package lambdas
package strings
package parser

import interpreters._
import safecast._
import trees._
import arithmetic._
import Extractors._
import cats.syntax.either._

case class StringsParser[T[_]: StringType: IntType: Cast, F[_]]()(
    implicit
    A: Strings[F],
    S: Forall0[T, cats.Show]
) extends OpenInterpreter[Tree, Either[String, DynTerm[T, F]]] {

  def apply(rec: => Interpreter[Tree, Either[String, DynTerm[T, F]]]) = {
    case AString(i) =>
      Right(DynTerm(StringType[T].tString, A.string(i)))

    case Length(s) =>
      for {
        ds <- rec(s)
        _ds <- ds
          .as(StringType[T].tString)
          .toEither(s"\nFirst operand of length, not an string: ${S().show(ds.typ)}")
      } yield DynTerm(IntType[T].tint, A.length(_ds))
    case t =>
      Left(s"\nNot an Strings term $t")
  }
}

object StringsParser {

  import trees.tfdbparser.ArrowParser.OpenInterpreterEither

  def forall[P[_, _]: Forall[?[_, _], Strings], T[_]: StringType: IntType: Cast: Forall0[
    ?[_],
    cats.Show
  ]] =
    new Forall[P, OpenInterpreterEither[T, ?[_]]] {
      def apply[E] = StringsParser[T, P[E, ?]]()
    }
}
