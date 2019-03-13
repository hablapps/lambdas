package lambdas
package interpreters

object Interpreter {

  def apply[A, E, B](
      p: PartialFunction[A, Either[E, B]]
  )(error: A => E): Interpreter[A, Either[E, B]] =
    (a: A) => p.applyOrElse(a, (a: A) => Left(error(a)))
}
