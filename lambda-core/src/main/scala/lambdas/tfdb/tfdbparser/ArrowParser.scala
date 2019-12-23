package lambdas
package trees
package tfdbparser

import tfdb._
import safecast._
import interpreters._
import lambdas.syntax._

import ArrowParser.Result

case class ArrowParser[P[_, _], T[_]: ArrowType: Cast](
    TypeParser: Interpreter[Tree, Either[String, ATypeTerm[T]]]
)(
    implicit
    AsArrow: ArrowType.Arrow1.Match[T],
    S: Forall0[T, cats.Show],
    L: Lambda[T, P]
) extends OpenInterpreter[Tree, Result[T, P]] {
  import cats.syntax.either._

  def apply(rec: => Interpreter[Tree, Result[T, P]]) =
    (tree: Tree) =>
      new Result[T, P] {
        def apply[Γ, E](γ: Γ)(implicit G: Gamma[Γ, E, T]) =
          tree match {
            case Var(name) =>
              G.findVar(name, γ)

            case Lam(name, typ, body) =>
              TypeParser(typ).flatMap { ty1 =>
                implicit val _ = ty1.typ
                rec(body)((Gamma.Var(name, ty1.typ), γ)).map { db =>
                  DynLTerm(ArrowType[T].tarrow(ty1.typ, db.typ), L.lam(db.term)(ty1.typ, db.typ))
                }
              }

            case Lam2(name, typ, name2, typ2, body) =>
              TypeParser(typ).flatMap { ty1 =>
                TypeParser(typ2).flatMap { ty2 =>
                  implicit val ty1typ = ty1.typ
                  implicit val ty2typ = ty2.typ
                  rec(body)((Gamma.Var(name2, ty2.typ), (Gamma.Var(name, ty1.typ), γ))).map { db =>
                    DynLTerm(
                      tarrow(ty1.typ, tarrow(ty2.typ, db.typ)),
                      L.lam2(db.term)(ty1.typ, ty2.typ, db.typ)
                    )
                  }
                }
              }

            case Curry(lam2) =>
              for {
                dl2  <- rec(lam2)(γ)
                asA1 <- AsArrow.unapply(dl2.typ).toEither(s"Not a lambda: ${S().show(dl2.typ)}")
                asA2 <- AsArrow.unapply(asA1.t2).toEither(s"Not a lambda: ${S().show(asA1.t2)}")
                curriedType = tarrow(asA1.t1, tarrow(asA2.t1, asA2.t2))
              } yield
                DynLTerm(
                  tarrow(asA1.t1, tarrow(asA2.t1, asA2.t2)),
                  asA2.as[λ[T => P[E, asA1.T1 => T]]](asA1.as[P[E, ?]](dl2.term))
                )

            case App(ft, at) =>
              for {
                df  <- rec(ft)(γ)
                asA <- AsArrow.unapply(df.typ).toEither(s"\nNot a lambda: ${S().show(df.typ)}")
                da  <- rec(at)(γ)
                _da <- da
                  .as(asA.t1)
                  .toEither(s"\nWrong type: ${S().show(da.typ)} should be ${S().show(asA.t1)}")
              } yield DynLTerm(asA.t2, L.app(asA.as[P[E, ?]](df.term))(_da)(asA.t1, asA.t2))

            case _ =>
              Left(s"\nNot a lambda term: $tree")
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

  type OpenInterpreterEither[T[_], F[_]] =
    OpenInterpreter[Tree, Either[String, DynTerm[T, F]]]

  case class Lifted[P[_, _], T[_]](
      Parser: Forall[P, OpenInterpreterEither[T, ?[_]]]
  ) extends OpenInterpreter[Tree, Result[T, P]] {
    def apply(rec: => Interpreter[Tree, Result[T, P]]) =
      (tree: Tree) =>
        new Result[T, P] {
          def apply[Γ, E](γ: Γ)(implicit G: Gamma[Γ, E, T]) =
            Parser[E].apply(rec andThen (_(γ)))(tree)
        }
  }

  import scala.language.implicitConversions

  implicit def lift[P[_, _], T[_]](
      parser: Forall[P, OpenInterpreterEither[T, ?[_]]]
  ) =
    Lifted(parser)

}
