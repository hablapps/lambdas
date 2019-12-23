package lambdas.safecast

trait Dynamic2[F[_, _]] {
  type S
  type A

  val value: F[S, A]
}

object Dynamic2 {
  def apply[F[_, _], _S, _A](v: F[_S, _A]): Dynamic2[F] = new Dynamic2[F] {
    override type S = _S
    override type A = _A

    override val value: F[_S, _A] = v
  }
}
