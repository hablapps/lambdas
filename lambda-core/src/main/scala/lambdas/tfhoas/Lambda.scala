package lambdas
package tfhoas

abstract class Lambda[Type[_]: ArrowType, P[_]] {

  def lam[A: Type, B: Type](f: P[A] => P[B]): P[A => B]

  def app[A: Type, B: Type](f: P[A => B])(t1: P[A]): P[B]

  // Products

  def tuple[A: Type, B: Type](a: P[A], b: P[B]): P[(A, B)]

  def fst[A: Type, B: Type](t: P[(A, B)]): P[A]

  def snd[A: Type, B: Type](t: P[(A, B)]): P[B]

  // Auxiliary

  def lam2[A: Type, B: Type, C: Type](f: (P[A], P[B]) => P[C]): P[(A, B) => C]

  def curried[A: Type, B: Type, C: Type](f: P[(A, B) => C]): P[A => B => C]

  def tupled[A: Type, B: Type, C: Type](f: P[(A, B) => C]): P[((A, B)) => C]

}

object Lambda {

  def apply[Type[_], P[_]](implicit L: Lambda[Type, P]) = L

  import trees.TreeSerializable
  implicit def _Serializer[Type[_]: ArrowType: TreeSerializable] = new semantics.Serializer[Type]
  implicit def _Show[Type[_]: ArrowType]                         = new semantics.ShowLambda[Type]
  implicit def _Standard[Type[_]: ArrowType]                     = new semantics.Standard[Type]
}
