package lambdas
package safecast

trait Typeable[T] {
  type Type[_]
  val T: Type[T]
}

object Typeable {
  type Aux[T, _Type[_]] = Typeable[T] { type Type[t] = _Type[t] }

  def apply[T, _Type[_]](_T: _Type[T]): Typeable.Aux[T, _Type] =
    new Typeable[T] {
      type Type[t] = _Type[t]
      val T = _T
    }
}
