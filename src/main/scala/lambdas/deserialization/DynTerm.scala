package lambdas
package deserialization

// import taglessfinal.debruijn._

abstract class DynTerm[P[_, _], E]{
  type A
  val typ: TQ[A]
  val term: P[E, A]
}

object DynTerm{
  type Aux[P[_, _], _A, E] = DynTerm[P, E]{ type A = _A }

  def unapply[P[_, _], E](dt: DynTerm[P, E]): Option[(TQ[_], P[E, _])] =
    Some((dt.typ, dt.term))

  def apply[P[_, _], _A, E](_typ: TQ[_A], _term: P[E, _A]) =
    new DynTerm[P, E]{
      type A = _A
      val typ = _typ
      val term = _term
    }
}
