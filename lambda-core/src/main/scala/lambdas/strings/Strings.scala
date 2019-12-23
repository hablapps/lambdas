package lambdas
package strings

import arithmetic.Num

trait Strings[P[_]] extends Serializable {

  def string(s: String): P[String]

  def length(s: P[String]): P[Num]
}

object Strings extends LPI {

  def apply[P[_]](implicit A: Strings[P]) = A

  implicit val IdStrings         = semantics.IdStrings
  implicit val ShowStrings       = semantics.ShowStrings
  implicit val SerializerStrings = semantics.SerializerStrings
  implicit val StringsFunction1  = semantics.Function1Strings

}

trait LPI {

  implicit def StringsForall[E, P[_, _]](implicit FA: Forall[P, Strings]): Strings[P[E, ?]] =
    FA[E]

  implicit def ForallStrings[P[_]](implicit P: Strings[P]) =
    new semantics.ForallOpenHOAS[P]
}
