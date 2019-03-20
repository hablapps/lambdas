package lambdas
package trees
package semantics2

import syntax._
import safecast2._
import interpreters._

import tfdb._
import arithmetic._, arithmetic.semantics.ShowArithFun

import LambdaTermParser.Result

object TermParser {

  case class IntTermParserLifted[T[_], P[_, _]]()(
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
            IntTermParser.parser[T, P[E, ?]](I, C, F[E], S)(rec andThen { _.apply(γ) })(tree)
        }
  }

  import cats.instances.string._

  implicit class Ops[T[_], P[_, _]](sem1: OpenInterpreter[Tree, Result[T, P]]) {
    def orElse(sem2: OpenInterpreter[Tree, Result[T, P]]) =
      new OpenInterpreter[Tree, Result[T, P]] {
        def apply(rec: => Tree => Result[T, P]): Tree => Result[T, P] =
          (tree: Tree) =>
            new Result[T, P] {
              def apply[Γ, E](γ: Γ)(implicit G: Gamma[Γ, E, T]) =
                sem1(rec)(tree).apply(γ) orElse
                sem2(rec)(tree).apply(γ)
            }
      }
  }

  def apply[P[_, _]: Lambda: ForAll[?[_, _], Arithmetic]]
    : Interpreter[Tree, Result[IntArrowTypeTerm, P]] =
    LambdaTermParser[P, IntArrowTypeTerm](TypeParser.apply) orElse
    IntTermParserLifted[IntArrowTypeTerm, P]() close
}
