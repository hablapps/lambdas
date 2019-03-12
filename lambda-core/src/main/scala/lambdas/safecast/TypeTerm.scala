package lambdas
package safecast

trait TypeTerm[T] {
  def apply[P[_]](implicit T: Type[P]): P[T]
}

object TypeTerm {

  implicit val TypeTermType = new Type[TypeTerm] {
    def tint: TypeTerm[Int] = new TypeTerm[Int] {
      def apply[P[_]](implicit T: Type[P]): P[Int] =
        T.tint
    }

    def tarr[T1, T2](t1: TypeTerm[T1], t2: TypeTerm[T2]): TypeTerm[T1 => T2] =
      new TypeTerm[T1 => T2] {
        def apply[P[_]](implicit T: Type[P]): P[T1 => T2] =
          T.tarr(t1[P], t2[P])
      }
  }
}
