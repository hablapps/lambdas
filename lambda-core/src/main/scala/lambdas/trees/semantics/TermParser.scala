package lambdas
package trees
package semantics

import syntax._
import safecast._
import interpreters._

import tfdb._
import arithmetic._, arithmetic.semantics.ShowArithFun

object TermParser {

  case class IntTermParserLifted[P[_, _]]()(implicit F: ForAll[P, Arithmetic])
      extends OpenInterpreter[Tree, ParsedLambdaTerm[P]] {
    def apply(rec: => Interpreter[Tree, ParsedLambdaTerm[P]]) =
      (tree: Tree) =>
        new ParsedLambdaTerm[P] {
          def apply[Γ, E](γ: Γ)(implicit G: Gamma[Γ, E]) =
            IntTermParser.parser[P[E, ?]](F[E])(rec andThen { _.apply(γ) })(tree)
        }
  }

  import cats.instances.string._

  implicit class Ops[P[_, _]](sem1: OpenInterpreter[Tree, ParsedLambdaTerm[P]]) {
    def orElse(sem2: OpenInterpreter[Tree, ParsedLambdaTerm[P]]) =
      new OpenInterpreter[Tree, ParsedLambdaTerm[P]] {
        def apply(rec: => Tree => ParsedLambdaTerm[P]): Tree => ParsedLambdaTerm[P] =
          (tree: Tree) =>
            new ParsedLambdaTerm[P] {
              def apply[Γ, E](γ: Γ)(implicit G: Gamma[Γ, E]) =
                sem1(rec)(tree).apply(γ) orElse
                sem2(rec)(tree).apply(γ)
            }
      }
  }

  def apply[P[_, _]: Lambda](
      implicit A: ForAll[P, Arithmetic]
  ): Interpreter[Tree, ParsedLambdaTerm[P]] =
    (ParsedLambdaTerm.Parser[P]: OpenInterpreter[Tree, ParsedLambdaTerm[P]]) orElse
    IntTermParserLifted[P]() close
}
