// package lambdas
// package trees
// package semantics2

// import tfdb._, syntax._
// import safecast.TypeTerm

// trait Gamma[Γ, E] {
//   def findVar[P[_, _]: Lambda](name: String, gamma: Γ): Either[String, DynLTerm[P, E]]
// }

// object Gamma {

//   case class Var[T](name: String, typ: TypeTerm[T])

//   implicit val notFound: Gamma[Unit, Unit] = new Gamma[Unit, Unit] {
//     def findVar[P[_, _]: Lambda](name: String, gamma: Unit): Either[String, DynLTerm[P, Unit]] =
//       Left(s"Var not found: $name")
//   }

//   implicit def foundVar[Γ, E, T](implicit G: Gamma[Γ, E]): Gamma[(Var[T], Γ), (T, E)] =
//     new Gamma[(Var[T], Γ), (T, E)] {

//       def findVar[P[_, _]: Lambda](
//           name: String,
//           gamma: (Var[T], Γ)
//       ): Either[String, DynLTerm[P, (T, E)]] =
//         gamma match {

//           case (Var(_, typ), _) =>
//             Right(DynLTerm(typ, vz[P, E, T]))

//           case (_, tail) =>
//             for {
//               dt <- G.findVar(name, tail).right
//             } yield DynLTerm(dt.typ, vs(dt.term))
//         }
//     }
// }
