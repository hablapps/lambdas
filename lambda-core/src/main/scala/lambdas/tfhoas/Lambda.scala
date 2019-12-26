package lambdas
package tfhoas

abstract class Lambda[Type[_]: ArrowType, P[_]] extends Serializable {

  def lam[A: Type, B: Type](f: P[A] => P[B]): P[A => B]

  def app[A: Type, B: Type](f: P[A => B])(t1: P[A]): P[B]
}

object Lambda {

  def apply[Type[_], P[_]](implicit L: Lambda[Type, P]) = L

  import trees.TreeSerializable
  implicit def _Serializer[Type[_]: ArrowType: TreeSerializable] = new semantics.Serializer[Type]
  implicit def _Show[Type[_]: ArrowType]                         = new semantics.ShowLambda[Type]
  implicit def _Standard[Type[_]: ArrowType]                     = new semantics.Standard[Type]
  implicit def _TDBLambda[Type[_]: ArrowType: safecast.Cast]     = new semantics.TDB.TDB_Hoas[Type]
  import semantics.GenLambda._

  implicit def _IsoHOAS[Type[_]: ArrowType, P[_]](
      implicit
      IsoArrow1: IsoGen2[P, Function1, GenArrow1[P, ?, ?]]
  ) = new IsoHOAS[Type, P]

}
