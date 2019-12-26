package lambdas
package tfdb
package semantics

abstract class ChurchDB[Type[_]: ArrowType, E, T] {
  def apply[P[_, _]](implicit L: Lambda[Type, P]): P[E, T]
}

object ChurchDB {

  class ChurchDB_DB[Type[_]: ArrowType] extends Lambda[Type, ChurchDB[Type, ?, ?]] {
    def vz[E, T: Type]: ChurchDB[Type, (T, E), T] =
      new ChurchDB[Type, (T, E), T] {
        def apply[P[_, _]](implicit L: Lambda[Type, P]): P[(T, E), T] =
          L.vz[E, T]
      }

    def vs[E, T: Type, T1: Type](a: ChurchDB[Type, E, T]): ChurchDB[Type, (T1, E), T] =
      new ChurchDB[Type, (T1, E), T] {
        def apply[P[_, _]](implicit L: Lambda[Type, P]): P[(T1, E), T] =
          L.vs[E, T, T1](a[P])
      }

    def lam[E, T1: Type, T2: Type](t: ChurchDB[Type, (T1, E), T2]): ChurchDB[Type, E, T1 => T2] =
      new ChurchDB[Type, E, T1 => T2] {
        def apply[P[_, _]](implicit L: Lambda[Type, P]): P[E, T1 => T2] =
          L.lam(t[P])
      }

    def app[E, T1: Type, T2: Type](f: ChurchDB[Type, E, T1 => T2])(
        t1: ChurchDB[Type, E, T1]
    ): ChurchDB[Type, E, T2] =
      new ChurchDB[Type, E, T2] {
        def apply[P[_, _]](implicit L: Lambda[Type, P]): P[E, T2] =
          L.app(f[P])(t1[P])
      }
  }
}
