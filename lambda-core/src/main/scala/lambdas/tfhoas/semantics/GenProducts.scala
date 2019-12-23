package lambdas
package tfhoas
package semantics

object GenProducts {

  type GenTuple2[P[_], A, B] = (P[A], P[B])

  type GenArrow1[P[_], A, B] = P[A] => P[B]

  class IsoHOAS[P[_]](
      implicit
      IsoProduct2: IsoGen2[P, Tuple2, GenTuple2[P, ?, ?]]
  ) extends Products[P] {

    def tuple[A, B](a: P[A], b: P[B]): P[(A, B)] =
      IsoProduct2[A, B].put((a, b))

    def fst[A, B](t: P[(A, B)]): P[A] =
      IsoProduct2[A, B].get(t)._1

    def snd[A, B](t: P[(A, B)]): P[B] =
      IsoProduct2[A, B].get(t)._2

  }
}
