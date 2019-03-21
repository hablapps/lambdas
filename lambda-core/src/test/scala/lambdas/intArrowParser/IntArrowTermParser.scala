package lambdas
package intArrowParser

import trees._, syntax._
import safecast._
import interpreters._
import tfdb._, tfdb.semantics._, ArrowParser._
import arithmetic._, arithmetic.semantics._

object IntArrowTermParser {

  case class IntParserLifted[P[_, _], T[_]]()(
      implicit
      F: ForAll[P, Arithmetic],
      S: ForAll0[T, cats.Show],
      I: IntType[T],
      C: Cast[T]
  ) extends OpenInterpreter[Tree, Result[T, P]] {
    def apply(rec: => Interpreter[Tree, Result[T, P]]) =
      (tree: Tree) =>
        new Result[T, P] {
          def apply[Γ, E](γ: Γ)(implicit G: Gamma[Γ, E, T]) =
            IntParser.parser[T, P[E, ?]].apply(rec andThen (_(γ)))(tree)
        }
  }

  def apply[P[_, _]: Lambda: ForAll[?[_, _], Arithmetic]]
    : Interpreter[Tree, Result[IntArrowTypeTerm, P]] =
    ArrowParser[P, IntArrowTypeTerm](IntArrowTypeTermParser.apply) orElse
    IntParserLifted[P, IntArrowTypeTerm]() close
}
