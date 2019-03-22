package lambdas
package tfdb
package semantics

import trees._, syntax._
import safecast._
import interpreters._

import tfdb._
import ArrowParser.Result

case class ArrowParser[P[_, _], T[_]: ArrowType: Cast](
    TypeParser: Interpreter[Tree, Either[String, ATypeTerm[T]]]
)(
    implicit
    AsArrow: ArrowType.Match[T],
    S: ForAll0[T, cats.Show],
    L: Lambda[P]
) extends OpenInterpreter[Tree, Result[T, P]] {

  def apply(rec: => Interpreter[Tree, Result[T, P]]) =
    (tree: Tree) =>
      new Result[T, P] {
        def apply[Γ, E](γ: Γ)(implicit G: Gamma[Γ, E, T]) =
          tree match {
            case Var(name) =>
              G.findVar(name, γ)

            case Lam(name, typ, body) =>
              for {
                ty1 <- TypeParser(typ)
                db  <- rec(body)((Gamma.Var(name, ty1.typ), γ))
              } yield DynLTerm(ArrowType[T].tarrow(ty1.typ, db.typ), L.lam(db.term))

            case App(ft, at) =>
              for {
                df  <- rec(ft)(γ)
                asA <- AsArrow.unapply(df.typ).toEither(s"Not a lambda: ${S().show(df.typ)}")
                da  <- rec(at)(γ)
                _da <- da
                  .as(asA.t1)
                  .toEither(s"Wrong type: ${S().show(da.typ)} should be ${S().show(asA.t1)}")
              } yield DynLTerm(asA.t2, L.app(asA.as[P[E, ?]](df.term))(_da))

            case _ =>
              Left(s"ParseTerm error: $tree")
          }
      }
}

object ArrowParser {

  trait Result[T[_], P[_, _]] {
    def apply[Γ, E](γ: Γ)(implicit G: Gamma[Γ, E, T]): Either[String, DynTerm[T, P[E, ?]]]
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

  case class Lifted[P[_, _], T[_]](
      Parser: ForAll[P, λ[F[_] => OpenInterpreter[Tree, Either[String, DynTerm[T, F]]]]]
  ) extends OpenInterpreter[Tree, Result[T, P]] {
    def apply(rec: => Interpreter[Tree, Result[T, P]]) =
      (tree: Tree) =>
        new Result[T, P] {
          def apply[Γ, E](γ: Γ)(implicit G: Gamma[Γ, E, T]) =
            Parser[E].apply(rec andThen (_(γ)))(tree)
        }
  }

  implicit def lift[P[_, _], T[_]](
      parser: ForAll[P, λ[F[_] => OpenInterpreter[Tree, Either[String, DynTerm[T, F]]]]]
  ) =
    Lifted(parser)

}
