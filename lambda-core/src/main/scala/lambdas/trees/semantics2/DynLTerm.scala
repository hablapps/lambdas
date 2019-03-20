// package lambdas
// package trees
// package semantics2

// import safecast._

// trait DynLTermModule {

//   type DynLTerm[P[_, _], E] = DynTerm[P[E, ?]]

//   object DynLTerm {
//     def unapply[P[_, _], E](dt: DynLTerm[P, E]): Option[(TypeTerm[_], P[E, _])] =
//       DynTerm.unapply[P[E, ?]](dt)

//     def apply[P[_, _], E, A](ta: TypeTerm[A], term: P[E, A]): DynLTerm[P, E] =
//       DynTerm[P[E, ?], A](ta, term)
//   }
// }
