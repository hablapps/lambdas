package lambdas.safecast

trait Dynamic[F[_]] {
  type A

  val value: F[A]
}

object Dynamic {
  def apply[F[_], _A](v: F[_A]): Dynamic[F] = new Dynamic[F] {
    override type A = _A

    override val value: F[_A] = v
  }
}
