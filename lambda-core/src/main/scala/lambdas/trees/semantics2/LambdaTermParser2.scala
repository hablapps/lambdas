// package lambdas
// package trees
// package semantics

// import syntax._
// import safecast._
// import interpreters._

// import tfdb._
// import cats.{ Functor, Semigroup }, cats.syntax.functor._, cats.syntax.either._

// trait Parser[A, F[_], E, B] { self =>
//   val AE: Error[F, E]

//   def apply(a: A): F[B]

//   def |[C](p: Parser[A, F, E, C])(
//       implicit
//       F: Functor[F],
//       S: Semigroup[E]
//   ): Parser[A, F, E, Either[B, C]] =
//     new Parser[A, F, E, Either[B, C]] {
//       val AE = self.AE

//       def apply(a: A): F[Either[B, C]] =
//         AE.handleError(self(a).map(Either.left[B, C])) { errorA =>
//           AE.handleError(p(a).map(Either.right[B, C])) { errorB =>
//             AE.raiseError[Either[B, C]](Semigroup[E].combine(errorA, errorB))
//           }
//         }
//     }
// }

// trait Error[F[_], E] {
//   def handleError[A](fa: F[A])(f: E => F[A]): F[A]
//   def raiseError[A](fa: E): F[A]
// }

// import arithmetic._

// abstract class ArithParser[P[_]: Arithmetic]
//     extends Parser[Tree, Either[String, ?], String, DynTerm[P]] {
//   def apply(a: Tree): Either[String, DynTerm[P]] = ???
// }

// abstract class DynLTerm2[P[_, _]] {
//   type Env
//   val dt: DynTerm[P[Env, ?]]
// }

// object DynLTerm2 {
//   type Aux[P[_, _], _Env] = DynLTerm2[P] { type Env = _Env }

//   def apply[P[_, _], _Env](_dt: DynLTerm[P, _Env]) = new DynLTerm2[P] {
//     type Env = _Env
//     val dt = _dt
//   }
// }

// import cats.evidence.Is

// abstract class DynLTerm3[P[_, _], _Env] extends DynLTerm2[P] {
//   type Env = _Env
//   val cast: Env Is _Env
// }

// object DynLTerm3 {

//   def apply[P[_, _], _Env](_dt: DynLTerm[P, _Env]) = new DynLTerm3[P, _Env] {
//     val dt                = _dt
//     val cast: _Env Is Env = Is.refl[Env]
//   }
// }

// abstract class EnvF[T] {
//   def apply[Γ, E](γ: Γ)(implicit G: Gamma[Γ, E]): Either[String, T]
// }

// object EnvF {

//   implicit object EnvFHandleError extends Error[EnvF, String] {
//     def handleError[A](fa: EnvF[A])(f: String => EnvF[A]): EnvF[A] =
//       new EnvF[A] {
//         def apply[Γ, E](γ: Γ)(implicit G: Gamma[Γ, E]): Either[String, A] =
//           fa(γ).fold(f andThen (_(γ)), Either.right)
//       }

//     def raiseError[A](error: String): EnvF[A] =
//       new EnvF[A] {
//         def apply[Γ, E](γ: Γ)(implicit G: Gamma[Γ, E]): Either[String, A] =
//           Either.left[String, A](error)
//       }
//   }
// }

// abstract class LambdaParser2[P[_, _]](implicit L: Lambda[P])
//     extends Parser[Tree, EnvF, String, DynLTerm3[P, _]] {
//   self =>
//   def apply(tree: Tree): EnvF[DynLTerm2[P]] =
//     new EnvF[DynLTerm3[P, _]] {
//       def apply[Γ, E](γ: Γ)(implicit G: Gamma[Γ, E]): Either[String, DynLTerm3[P, E]] =
//         tree match {
//           case Var(name) =>
//             ??? // G.findVar(name, γ) map (DynLTerm2(_))

//           case Lam(name, typ, body) =>
//             for {
//               ty1 <- TypeParser.apply(typ)
//               db <- self(body)
//                 .apply[(Gamma.Var[ty1.A], Γ), (ty1.A, E)]((Gamma.Var(name, ty1.typ), γ))
//             } yield
//               DynLTerm3(
//                 DynLTerm(
//                   ty1.typ -> db.dt.typ,
//                   L.lam[E, ty1.A, db.dt.A](db.cast.substitute[P[?, db.dt.A]](db.dt.term))
//                 )
//               )

//           case App(ft, at) =>
//             ???
//           // for {
//           //   df  <- self(ft).apply(γ)
//           //   asA <- df.dt.asArrow.toEither(s"Not a lambda: ${df.dt.typ[ShowP]}")
//           //   da  <- self(at).apply(γ)
//           //   _da <- da.dt
//           //     .as(asA.typ1)
//           //     .toEither(s"Wrong type: ${da.dt.typ[ShowP]} should be ${asA.typ1[ShowP]}")
//           // } yield DynLTerm2(DynLTerm(asA.typ2, L.app(asA.term)(_da)))

//           case _ =>
//             Left(s"ParseTerm error: $tree")
//         }
//     }
// }
