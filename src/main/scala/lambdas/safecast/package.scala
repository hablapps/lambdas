package lambdas

// http://okmij.org/ftp/tagless-final/course/TypeCheck.hs
// http://okmij.org/ftp/tagless-final/course/Typ.hs

package object safecast extends Type.Syntax {

  type Show[T] = String

  // Auxiliary function

  import cats.evidence.Is

  implicit class Lift2Op[A, A2, B, B2](p: (A Is A2, B Is B2)) {
    def lift2[T[_, _]]: T[A, B] Is T[A2, B2] =
      p._2.substitute[λ[X => T[A, B] Is T[A2, X]]](
        p._1.substitute[λ[X => T[A, B] Is T[X, B]]](Is.refl)
      )
  }
}
