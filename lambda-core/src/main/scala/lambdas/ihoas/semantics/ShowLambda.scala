package lambdas
package initial
package hoas
package semantics

object ShowLambda {

  def apply[Type[_]: ArrowType, T](l: Lambda[Type, Show, T]): Show[T] =
    FinallyLambda[Type, Show].apply(l)
}
