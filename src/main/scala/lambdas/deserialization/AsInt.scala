package lambdas
package deserialization

import scalaz.Leibniz._

case class AsInt[A](eq: Option[A === Int])

object AsInt{


  implicit def apply[P[_, _], A, E](asInt: AsInt[A]): P[E, A] => Option[P[E, Int]] =
    pa => asInt.eq.map(_.subst[P[E, ?]](pa))

  // OMG!, this also works!!!!
  // implicit def apply[P[_], A](asInt: AsInt[A]): P[A] => Option[P[Int]] =
  //   pa => asInt.eq.map(_.subst[P](pa))

  implicit val AsIntTSYM = new TSYM[AsInt]{
    def tint: AsInt[Int] =
      AsInt[Int](Option(refl))

    def tarr[T1, T2](t1: AsInt[T1], t2: AsInt[T2]): AsInt[T1 => T2] =
      AsInt[T1 => T2](None)
  }
}
