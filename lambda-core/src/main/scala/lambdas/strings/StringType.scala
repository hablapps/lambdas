package lambdas
package strings

trait StringType[T[_]] extends Serializable {
  def tString: T[String]
}

object StringType
    extends StringTypeImplicits
    with StringTypeSerialization
    with StringTypeDeserialization
    with StringTypeLPI {

  def apply[T[_]](implicit T: StringType[T]) = T
}

trait StringTypeImplicits {

  trait Implicits[T[_]] {
    val _StringType: StringType[T]

    implicit val _String: T[String] =
      Implicits._String(_StringType)
  }

  object Implicits {
    implicit def _String[T[_]](implicit T: StringType[T]): T[String] =
      T.tString
  }
}

trait StringTypeDeserialization extends StringTypeDeserializationLPI {
  import cats.evidence._
  import safecast._

  case class Case[A](is: A Is String)

  implicit val _StringTypeCase = new StringType[λ[T => Option[Case[T]]]] {
    def tString: Option[Case[String]] =
      Option(Case(Is.refl[String]))
  }

  type Match[T[_]] = safecast.Match[T, Case]

  implicit def _CastAsStringType[T[_]: StringType](implicit Is: Match[T]) =
    new StringType[Cast.As[T, ?]] {
      def tString = new Cast.As[T, String] {
        def apply[T2](t2: T[T2]): Option[String Is T2] =
          Is.unapply(t2) map (_.is.flip)
      }
    }
}

trait StringTypeDeserializationLPI {

  implicit def _NoneStringTypeCase[T[_]: StringType, Ca[T[_], _]] =
    new StringType[λ[A => (T[A], Option[Ca[T, A]])]] {
      def tString: (T[String], Option[Ca[T, String]]) =
        (StringType[T].tString, None)
    }

  implicit def StringTypeCaseNoneGen1[Ca[_]] =
    new StringType[λ[A => Option[Ca[A]]]] {
      def tString: Option[Ca[String]] =
        None
    }
}

trait StringTypeSerialization {
  import trees._, TreeSerializable.ShowTree

  trait Constructors {
    def tr_tString: Tree =
      Leaf("TString")
  }

  object Constructors extends Constructors

  implicit def _ShowTree = new StringType[ShowTree] {
    def tString: Int => Tree =
      _ => Constructors.tr_tString
  }
}

trait StringTypeLPI {

  implicit val _ShowP = new StringType[ShowP] {
    def tString: String = "TString"
  }
}

trait StringTypeSyntax {
  // def tString[T[_]](implicit T: StringType[T]): T[String] =
  //   T.tString
}
