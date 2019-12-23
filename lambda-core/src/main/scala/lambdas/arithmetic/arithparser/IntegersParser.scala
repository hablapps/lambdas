package lambdas
package trees
package arithparser

import safecast._
import interpreters._
import arithmetic._

case class IntegerParser[T[_]: IntType: Cast: Forall0[?[_], cats.Show], F[_]]()(
    implicit
    A: Integers[F]
) extends OpenInterpreter[Tree, Either[String, DynTerm[T, F]]] {

  def apply(rec: => Interpreter[Tree, Either[String, DynTerm[T, F]]]) = {
    case IntT(i) =>
      Right(DynTerm(IntType[T].tint, A.int(i)))
    case t => Left(s"\nNot an int term $t")
  }
}

object IntegerParser {

  import trees.tfdbparser.ArrowParser.OpenInterpreterEither

  def forall[P[_, _]: Forall[?[_, _], Integers], T[_]: IntType: Cast: Forall0[?[_], cats.Show]] =
    new Forall[P, OpenInterpreterEither[T, ?[_]]] {
      def apply[E] = IntegerParser[T, P[E, ?]]()
    }
}
