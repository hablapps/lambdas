package lambdas
package arithmetic

// TODO: Redundant, should be removed
trait Integers[P[_]] {

  def int(i: Num): P[Num]

}

object Integers extends IntegersLPI {

  def apply[P[_]](implicit I: Integers[P]) = I

  import cats.data.Const

  implicit object ShowIntegers extends Integers[Const[String, ?]] {

    def int(i: Num): Const[String, Num] =
      Const(i.toString)
  }

  import trees._, TreeSerializable.ShowTree

  implicit object SerializerIntegers extends Integers[ShowTree] {

    import Constructors._

    def int(value: Num): ShowTree[Num] =
      _ => tr_int(value)

    object Constructors {
      def tr_int(i: Num): Tree =
        Node("Int", List(Leaf(i.toString)))
    }
  }

  import TreeSerializable.ShowTreeB

  implicit object SerializerB extends Forall[ShowTreeB, Integers] {
    def apply[E] = new Integers[ShowTreeB[E, ?]] {

      def int(value: Num): ShowTreeB[E, Num] =
        Integers[ShowTree].int(value)
    }
  }

  import tfdb.semantics.OpenHOAS

  implicit def ForallOpenHOAS[P[_]](implicit R: Integers[P]): Forall[OpenHOAS[P, ?, ?], Integers] =
    new Forall[OpenHOAS[P, ?, ?], Integers] {
      def apply[E] = new Integers[OpenHOAS[P, E, ?]] {

        def int(value: Num) = OpenHOAS { _ =>
          R.int(value)
        }
      }
    }

  implicit def IsoIntegers[P[_]](implicit Setter: SetterGen[P, Num, Num]) =
    new Integers[P] {
      def int(i: Num): P[Num] =
        Setter().put(i)
    }
}

trait IntegersLPI {

  implicit def IntegersForall[E, P[_, _]](implicit FA: Forall[P, Integers]): Integers[P[E, ?]] =
    FA[E]
}
