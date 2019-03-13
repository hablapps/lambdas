package lambdas
package trees
package semantics

import syntax._
import safecast._
import interpreters._

import tfdb._
import arithmetic._, arithmetic.semantics.ShowArithFun

object ParseTerm2 {
  import cats.instances.string._

  trait ForAll[P[_, _], TC[_[_]]] {
    def apply[E]: TC[P[E, ?]]
  }

  object ForAll {
    implicit def ForAllLiftArithmeticShowB = new ForAll[ShowB, Arithmetic] {
      def apply[E] = new Arithmetic[ShowB[E, ?]] {
        def int(i: Int): ShowB[E, Int] =
          ShowArithFun.int(i)

        def add(i1: ShowB[E, Int])(i2: ShowB[E, Int]): ShowB[E, Int] =
          ShowArithFun.add(i1)(i2)

        def * : ShowB[E, (Int, Int) => Int] =
          ShowArithFun.*

        def + : ShowB[E, (Int, Int) => Int] =
          ShowArithFun.+
      }
    }
  }

  case class IntTermParserLifted[P[_, _]]()(implicit F: ForAll[P, Arithmetic])
      extends Interpreter[Tree, ParsedLambdaTerm[P]] {
    def apply(tree: Tree) =
      new ParsedLambdaTerm[P] {
        def apply[Γ, E](implicit G: Gamma[Γ, E]) =
          (_: Γ) => IntTermParser[P[E, ?]]()(F[E]).apply(tree)
      }
  }

  implicit class Ops[P[_, _]](sem1: OpenInterpreter[Tree, ParsedLambdaTerm[P]]) {
    def orElse(sem2: Interpreter[Tree, ParsedLambdaTerm[P]]) =
      new OpenInterpreter[Tree, ParsedLambdaTerm[P]] {
        def apply(rec: => Tree => ParsedLambdaTerm[P]): Tree => ParsedLambdaTerm[P] =
          (tree: Tree) =>
            new ParsedLambdaTerm[P] {
              def apply[Γ, E](implicit G: Gamma[Γ, E]) =
                (gamma: Γ) =>
                  sem1(rec)(tree).apply.apply(gamma) orElse
                  sem2(tree).apply.apply(gamma)
            }
      }
  }

  def apply[P[_, _]: Lambda](
      implicit A: ForAll[P, Arithmetic]
  ): Interpreter[Tree, ParsedLambdaTerm[P]] =
    (ParsedLambdaTerm.Parser[P]: OpenInterpreter[Tree, ParsedLambdaTerm[P]]) orElse
    IntTermParserLifted[P]() close
}
