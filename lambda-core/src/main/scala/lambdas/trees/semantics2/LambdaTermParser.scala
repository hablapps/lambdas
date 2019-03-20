package lambdas
package trees
package semantics2

import syntax._
import safecast2._
import interpreters._

import tfdb._
import LambdaTermParser.Result

case class LambdaTermParser[P[_, _], T[_]: ArrowType: Cast](
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
                ty1 <- TypeParser.apply(typ)
                db <- rec(body)
                  .apply[(Gamma.Var[T, ty1.A], Γ), (ty1.A, E)]((Gamma.Var(name, ty1.typ), γ))
              } yield DynLTerm(ArrowType[T].tarrow(ty1.typ, db.typ), L.lam(db.term))

            case App(ft, at) =>
              for {
                df  <- rec(ft).apply(γ)
                asA <- AsArrow.unapply(df.typ).toEither(s"Not a lambda: ${S().show(df.typ)}")
                da  <- rec(at).apply(γ)
                _da <- da
                  .as(asA.t1)
                  .toEither(s"Wrong type: ${S().show(da.typ)} should be ${S().show(asA.t1)}")
              } yield DynLTerm(asA.t2, L.app(asA.as[P[E, ?]](df.term))(_da))

            case _ =>
              Left(s"ParseTerm error: $tree")
          }
      }
}

object LambdaTermParser {

  trait Result[T[_], P[_, _]] {
    def apply[Γ, E](γ: Γ)(implicit G: Gamma[Γ, E, T]): Either[String, DynTerm[T, P[E, ?]]]
  }

}
