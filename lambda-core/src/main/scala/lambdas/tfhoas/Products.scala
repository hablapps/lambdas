package lambdas
package tfhoas

abstract class Products[P[_]] extends Serializable {
  def tuple[A, B](a: P[A], b: P[B]): P[(A, B)]

  def fst[A, B](t: P[(A, B)]): P[A]

  def snd[A, B](t: P[(A, B)]): P[B]
}

object Products {

  def apply[P[_]](implicit P: Products[P]) = P
  implicit val _Serializer                 = semantics.ProductsSerializer
  implicit val _Show                       = semantics.ShowProducts
  implicit val _Standard                   = semantics.StandardProducts

  import semantics.GenProducts._

  implicit def _IsoHOAS[P[_]](
      implicit
      IsoProduct2: IsoGen2[P, Tuple2, GenTuple2[P, ?, ?]]
  ) = new IsoHOAS[P]

}
