package lambdas
package deserialization

trait TQ[T]{
  def apply[P[_]](implicit T: TSYM[P]): P[T]
}

object TQ{

  implicit val TQTSYM = new TSYM[TQ]{
    def tint: TQ[Int] = new TQ[Int]{
      def apply[P[_]](implicit T: TSYM[P]): P[Int] =
        T.tint
    }

    def tarr[T1, T2](t1: TQ[T1], t2: TQ[T2]): TQ[T1 => T2] = new TQ[T1 => T2]{
      def apply[P[_]](implicit T: TSYM[P]): P[T1 => T2] =
        T.tarr(t1[P], t2[P])
    }
  }
}
