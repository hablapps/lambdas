package lambdas
package strings
package semantics

import arithmetic.Num

object Function1Strings extends Forall[Function1, Strings] {
  def apply[E] = new Strings[Function1[E, ?]] {
    override def string(s: String): Function1[E, String] =
      _ => s

    override def length(s: E => String): E => Num =
      env => s(env).length
  }
}
