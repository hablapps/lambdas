package lambdas
package initial
package hoas
package semantics

object ShowLambda {

  def apply[T](l: Lambda[Show, T]): Show[T] =
    FinallyLambda[Show].apply(l)
}
