package lambdas
package deserialization

abstract class Typ{
  type A
  val typ: TQ[A]
}

object Typ{
  def apply[_A](t: TQ[_A]): Typ = new Typ{
    type A = _A
    val typ = t
  }
}

