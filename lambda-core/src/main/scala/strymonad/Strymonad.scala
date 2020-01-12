package lambdas
package strymonad

trait Strymonad[Data[_], Str[_], Obs[_]] {

  // Producers

  def ofArr[A](data: Data[Array[A]]): Str[A]
  def unfold[B, A](step: Data[B] => Data[Option[(B, A)]], seed: Data[B]): Str[A]

  // Transformers

  def map[A, B](f: Data[A] => Data[B])(str: Str[A]): Str[B]
  def filter[A](pred: Data[A] => Data[Boolean])(str: Str[A]): Str[A]
  def take[A](count: Data[Int])(str: Str[A]): Str[A]
  def flatMap[A, B](f: Data[A] => Str[B])(str: Str[A]): Str[B]
  def zipWith[A, B, C](f: (Data[A], Data[B]) => Data[C])(str1: Str[A], str2: Str[B]): Str[C]
  def fold[A, B](nil: Data[B], cons: (Data[B], Data[A]) => Data[B])(str: Str[A]): Str[B]

  // Consumer

  def runFold[A, B](nil: Data[B], cons: (Data[B], Data[A]) => Data[B])(str: Str[A]): Obs[B]
}

object Strymonad {
  def apply[Cod[_], Str[_], Obs[_]](implicit S: Strymonad[Cod, Str, Obs]) = S

  implicit val AkkaStreamsIsStrymonad = semantics.AkkaStreams
  implicit val ShowIsStrymonad        = semantics.ShowStrymonad
}
