package lambdas
package tfhoas
package semantics

import safecast._
import tfdb.semantics.ChurchDB
import TDB.Ctx

abstract class TDB[Type[_]: ArrowType, A] {
  def apply[E](ctx: Ctx[Type, E]): ChurchDB[Type, E, A]
}

object TDB {

  implicit class TDBOps[Type[_]: ArrowType, A](tdb: TDB[Type, A]) {
    def church: ChurchDB[Type, Unit, A] =
      tdb(CtxZ())
  }

  sealed abstract class Ctx[Type[_], E1] {
    def len: Int
    def tshift1[P[_, _], A, E](n: Int, ctx: Ctx[Type, (A, E)])(
        implicit L: tfdb.Lambda[Type, P],
        Cast: Cast[Type]
    ): P[E1, A]
  }

  case class CtxZ[Type[_]]() extends Ctx[Type, Unit] {
    def len: Int = 0
    def tshift1[P[_, _], A, E](n: Int, ctx: Ctx[Type, (A, E)])(
        implicit L: tfdb.Lambda[Type, P],
        Cast: Cast[Type]
    ): P[Unit, A] =
      ???
  }

  case class CtxS[Type[_], E1, A1](ctx1: Ctx[Type, E1], tA1: Type[A1]) extends Ctx[Type, (A1, E1)] {
    def len: Int = ctx1.len + 1
    def tshift1[P[_, _], A, E](n: Int, c2: Ctx[Type, (A, E)])(
        implicit L: tfdb.Lambda[Type, P],
        Cast: Cast[Type]
    ): P[(A1, E1), A] = {
      val CtxS(_, tA) = c2
      if (n == 0)
        Cast
          .as[A1, A, λ[T => P[(A1, E1), T]]](tA1, tA)(L.vz(tA1))
          .get // .asInstanceOf[P[(A1, E1), A]]
      else L.vs(ctx1.tshift1(n - 1, c2))(tA, tA1)
    }
  }

  object Ctx {

    def tshift[Type[_]: ArrowType: Cast, A, E, E1, P[_, _]](
        ctxj: Ctx[Type, E1],
        ctxi: Ctx[Type, (A, E)]
    )(
        implicit
        L: tfdb.Lambda[Type, P]
    ): P[E1, A] =
      ctxj.tshift1(ctxj.len - ctxi.len, ctxi)
  }

  class TDB_Hoas[Type[_]: ArrowType: Cast] extends Lambda[Type, TDB[Type, ?]] {

    def lam[A, B](
        f: TDB[Type, A] => TDB[Type, B]
    )(implicit tA: Type[A], tB: Type[B]): TDB[Type, A => B] =
      new TDB[Type, A => B] {
        def apply[E](ctxi: Ctx[Type, E]): ChurchDB[Type, E, A => B] =
          new ChurchDB[Type, E, A => B] {
            def apply[P[_, _]](implicit L: tfdb.Lambda[Type, P]) = {
              val tdba: TDB[Type, A] = new TDB[Type, A] {
                def apply[E1](ctxj: Ctx[Type, E1]): ChurchDB[Type, E1, A] =
                  new ChurchDB[Type, E1, A] {
                    def apply[P1[_, _]](implicit L1: tfdb.Lambda[Type, P1]) =
                      Ctx.tshift[Type, A, E, E1, P1](ctxj, CtxS[Type, E, A](ctxi, tA))
                  }
              }
              L.lam(f(tdba)(CtxS[Type, E, A](ctxi, tA))(L))
            }
          }
      }

    def app[A: Type, B: Type](f: TDB[Type, A => B])(t1: TDB[Type, A]): TDB[Type, B] =
      new TDB[Type, B] {
        def apply[E](ctx: Ctx[Type, E]): ChurchDB[Type, E, B] =
          new ChurchDB[Type, E, B] {
            def apply[P[_, _]](implicit L: tfdb.Lambda[Type, P]) =
              L.app(f(ctx)(L))(t1(ctx)(L))
          }
      }
  }

}
