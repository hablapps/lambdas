package lambdas
package deserialization

// import tfdb._

abstract class DynTerm[P[_, _], E]{
  type A
  val typ: TQ[A]
  val term: P[E, A]

  def as[B](tb: TQ[B]): Option[P[E, B]] =
    typ[As].cast[B, P[E, ?]](tb, term)

  def asInt: Option[P[E, Int]] =
    as(tint[TQ])
    // typ[AsInt].cast[P[E, ?]](term)

  abstract class _AsArrow{
    type T1
    type T2
    val typ1: TQ[T1]
    val typ2: TQ[T2]
    val term: P[E, T1 => T2]
  }

  def asArrow: Option[_AsArrow] = {
    val asArrow = typ[AsArrow]
    for {
      (_typ1, _typ2, _term) <- asArrow.cast[P[E, ?]](term)
    } yield new _AsArrow{
      type T1 = asArrow.T1
      type T2 = asArrow.T2
      val typ1 = _typ1
      val typ2 = _typ2
      val term = _term
    }
  }
}

object DynTerm{
  type Aux[P[_, _], _A, E] = DynTerm[P, E]{ type A = _A }

  def unapply[P[_, _], E](dt: DynTerm[P, E]): Option[(TQ[_], P[E, _])] =
    Some((dt.typ, dt.term))

  def apply[P[_, _], _A, E](_typ: TQ[_A], _term: P[E, _A]): DynTerm[P, E] =
    new DynTerm[P, E]{
      type A = _A
      val typ = _typ
      val term = _term
    }
}
