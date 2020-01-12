package lambdas

trait ProductType[T[_]] extends Serializable {
  def tProduct[T1, T2](t1: T[T1], t2: T[T2]): T[(T1, T2)]
}

object ProductType
    extends ProductTypeImplicits
    with ProductTypeDeserialization
    with ProductTypeSerialization
    with ProductTypeLPI {

  def apply[T[_]](implicit A: ProductType[T]) = A
}

trait ProductTypeImplicits {

  trait Implicits[T[_]] {
    val _ProductType: ProductType[T]

    implicit def _Product[A, B](
        implicit
        TA: T[A],
        TB: T[B]
    ): T[(A, B)] =
      Implicits._Product(TA, TB, _ProductType)
  }

  object Implicits {
    implicit def _Product[T[_], A, B](
        implicit
        TA: T[A],
        TB: T[B],
        AT: ProductType[T]
    ): T[(A, B)] =
      AT.tProduct(TA, TB)
  }
}

trait ProductTypeDeserialization extends ProductTypeDeserializationLPI {
  import cats.evidence._
  import safecast._

  abstract class Case[T[_], A] {
    type T1
    type T2

    val t1: T[T1]
    val t2: T[T2]
    val is: A Is ((T1, T2))

    def as[F[_]](t: F[A]): F[(T1, T2)] =
      is.substitute[F](t)
  }

  implicit def Case_ProductType[T[_]: ProductType] =
    new ProductType[λ[A => (T[A], Option[Case[T, A]])]] {
      def tProduct[_T1, _T2](
          ot1: (T[_T1], Option[Case[T, _T1]]),
          ot2: (T[_T2], Option[Case[T, _T2]])
      ) =
        (ProductType[T].tProduct(ot1._1, ot2._1), Option(new Case[T, (_T1, _T2)] {
          type T1 = _T1
          type T2 = _T2

          val t1 = ot1._1
          val t2 = ot2._1
          val is = Is.refl[(T1, T2)]
        }))
    }

  type Match[T[_]] = safecast.Match[T, Case[T, ?]]

  implicit def ProductTypeCast[T[_]: ProductType](implicit IsProduct: Match[T]) =
    new ProductType[Cast.As[T, ?]] {
      def tProduct[T0, T1](t0: Cast.As[T, T0], t1: Cast.As[T, T1]) = new Cast.As[T, (T0, T1)] {
        def apply[T2](t2: T[T2]): Option[(T0, T1) Is T2] =
          for {
            result <- IsProduct.unapply(t2)
            eqT0   <- t0(result.t1)
            eqT1   <- t1(result.t2)
          } yield (eqT0, eqT1).lift2[Tuple2].andThen(result.is.flip)
      }
    }
}

trait ProductTypeDeserializationLPI {

  implicit def Case_ProductTypeGen2[T[_]: ProductType, Ca[T[_], A]] =
    new ProductType[λ[A => (T[A], Option[Ca[T, A]])]] {
      def tProduct[_T1, _T2](
          ot1: (T[_T1], Option[Ca[T, _T1]]),
          ot2: (T[_T2], Option[Ca[T, _T2]])
      ) =
        (ProductType[T].tProduct(ot1._1, ot2._1), None)
    }

  implicit def Case_ProductTypeGen1[Ca[_]] =
    new ProductType[λ[A => Option[Ca[A]]]] {
      def tProduct[_T1, _T2](
          ot1: Option[Ca[_T1]],
          ot2: Option[Ca[_T2]]
      ) = None
    }
}

trait ProductTypeSerialization {

  import trees._, TreeSerializable.ShowTree

  trait Constructors {
    def tr_tProduct(t1: Tree, t2: Tree): Tree =
      Node("TProduct", List(t1, t2))
  }

  object Constructors extends Constructors

  implicit def _ShowTree = new ProductType[ShowTree] {
    def tProduct[T1, T2](t1: ShowTree[T1], t2: ShowTree[T2]): ShowTree[(T1, T2)] =
      (i: Int) => Constructors.tr_tProduct(t1(i), t2(i))
  }
}

trait ProductTypeLPI {

  implicit val _ShowP = new ProductType[ShowP] {
    def tProduct[T1, T2](t1: String, t2: String): String =
      s"($t1, $t2)"
  }
}

trait ProductTypeSyntax {

  def tProduct[T[_], T1, T2](t1: T[T1], t2: T[T2])(implicit T: ProductType[T]): T[(T1, T2)] =
    T.tProduct(t1, t2)
}
