package lambdas
package trees
package semantics2

import safecast2._

trait DynLTermModule {

  type DynLTerm[T[_], P[_, _], E] = DynTerm[T, P[E, ?]]

  object DynLTerm {
    def unapply[T[_], P[_, _], E](dt: DynLTerm[T, P, E]): Option[(T[_], P[E, _])] =
      DynTerm.unapply[T, P[E, ?]](dt)

    def apply[T[_], P[_, _], E, A](ta: T[A], term: P[E, A]): DynLTerm[T, P, E] =
      DynTerm[T, P[E, ?], A](ta, term)
  }
}
