package lambdas
package interpreters

trait OpenInterpreter[A, B] {

  def apply(f: => Interpreter[A, B]): Interpreter[A, B]

  def close: Interpreter[A, B] =
    fix(apply)
}

object OpenInterpreter {

  def apply[A, E, B](
      f: (=> Interpreter[A, Either[E, B]]) => PartialFunction[A, Either[E, B]]
  )(error: A => E) =
    new OpenInterpreter[A, Either[E, B]] {
      def apply(i: => Interpreter[A, Either[E, B]]): Interpreter[A, Either[E, B]] =
        a => f(i).applyOrElse(a, (a: A) => Left(error(a)))
    }

  import cats.Semigroup

  implicit class Ops[A, E: Semigroup, B](sem1: OpenInterpreter[A, Either[E, B]]) {
    def orElse(sem2: OpenInterpreter[A, Either[E, B]]) =
      new OpenInterpreter[A, Either[E, B]] {
        def apply(rec: => A => Either[E, B]): A => Either[E, B] =
          a => sem1(rec)(a) orElse sem2(rec)(a)
      }

    def orElse(sem2: Interpreter[A, Either[E, B]]) =
      new OpenInterpreter[A, Either[E, B]] {
        def apply(rec: => A => Either[E, B]): A => Either[E, B] =
          a => sem1(rec)(a) orElse sem2(a)
      }
  }

}
