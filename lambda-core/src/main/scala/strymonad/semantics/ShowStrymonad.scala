package lambdas
package strymonad
package semantics

object ShowStrymonad extends Strymonad[Show, Show, Show] {

  // Producers

  def ofArr[A](data: Show[Array[A]]): Show[A] =
    i => s"ofArr(${data(i)})"

  def unfold[B, A](step: Show[B] => Show[Option[(B, A)]], seed: Show[B]): Show[A] =
    ???

  // Transformers

  def map[A, B](f: Show[A] => Show[B])(str: Show[A]): Show[B] =
    i => {
      val x = "x" + i
      "map(" + x + " => " + f((_: Int) => x)(i + 1) + ")(" + str(i) + ")"
    }

  def filter[A](pred: Show[A] => Show[Boolean])(str: Show[A]): Show[A] =
    ???

  def take[A](count: Show[Int])(str: Show[A]): Show[A] =
    ???

  def flatMap[A, B](f: Show[A] => Show[B])(str: Show[A]): Show[B] =
    ???

  def zipWith[A, B, C](f: (Show[A], Show[B]) => Show[C])(str1: Show[A], str2: Show[B]): Show[C] =
    ???

  def fold[A, B](nil: Show[B], cons: (Show[B], Show[A]) => Show[B])(str: Show[A]): Show[B] =
    ???

  // Consumer

  def runFold[A, B](nil: Show[B], cons: (Show[B], Show[A]) => Show[B])(str: Show[A]): Show[B] =
    i => {
      val xi     = "x" + i
      val `xi+1` = "x" + i + 1
      "fold(" + nil(i) + ", " + "(" + xi + ", " + `xi+1` + ") => " +
      cons((_: Int) => xi, (_: Int) => `xi+1`)(i + 2) + ")(" + str(i) + ")"
    }
}
