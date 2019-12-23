package lambdas

import cats.evidence._
import safecast._

trait ArrowType[T[_]] extends Serializable {
  def tarrow[T1, T2](t1: T[T1], t2: T[T2]): T[T1 => T2]
}

object ArrowType
    extends ArrowTypeImplicits
    with ArrowTypeDeserialization
    with ArrowTypeSerialization
    with ArrowTypeLPI {

  def apply[T[_]](implicit A: ArrowType[T]) = A
}

trait ArrowTypeImplicits {

  trait Implicits[T[_]] {
    val _ArrowType: ArrowType[T]

    implicit def _Function1[A, B](
        implicit
        TA: T[A],
        TB: T[B]
    ): T[A => B] =
      Implicits._Function1(TA, TB, _ArrowType)
  }

  object Implicits {
    implicit def _Function1[T[_], A, B](
        implicit
        TA: T[A],
        TB: T[B],
        AT: ArrowType[T]
    ): T[A => B] =
      AT.tarrow(TA, TB)
  }
}

trait ArrowTypeDeserialization extends ArrowTypeDeserialization2LPI {

  object Arrow1 {
    abstract class Case[T[_], A] {
      type T1
      type T2

      val t1: T[T1]
      val t2: T[T2]
      val is: A Is (T1 => T2)

      def as[F[_]](t: F[A]): F[T1 => T2] =
        is.substitute[F](t)
    }

    object Case {
      def unapply[T[_], A, _T1, _T2](c: Case[T, A] { type T1 = _T1; type T2 = _T2 })
        : Option[(T[_T1], T[_T2])] =
        Some((c.t1, c.t2))
    }

    type Match[T[_]] = safecast.Match[T, Case[T, ?]]
  }

  implicit def Case_Arrow1Type[T[_]: ArrowType] =
    new ArrowType[λ[A => (T[A], Option[Arrow1.Case[T, A]])]] {
      def tarrow[_T1, _T2](
          ot1: (T[_T1], Option[Arrow1.Case[T, _T1]]),
          ot2: (T[_T2], Option[Arrow1.Case[T, _T2]])
      ) =
        (ArrowType[T].tarrow(ot1._1, ot2._1), Option(new Arrow1.Case[T, _T1 => _T2] {
          type T1 = _T1
          type T2 = _T2

          val t1 = ot1._1
          val t2 = ot2._1
          val is = Is.refl[T1 => T2]
        }))
    }

  implicit def ArrowTypeCast[T[_]: ArrowType](
      implicit IsArrow1: Arrow1.Match[T]
  ) =
    new ArrowType[Cast.As[T, ?]] {
      def tarrow[T0, T1](t0: Cast.As[T, T0], t1: Cast.As[T, T1]) = new Cast.As[T, T0 => T1] {
        def apply[T2](t2: T[T2]): Option[(T0 => T1) Is T2] =
          for {
            result <- IsArrow1.unapply(t2)
            eqT0   <- t0(result.t1)
            eqT1   <- t1(result.t2)
          } yield (eqT0, eqT1).lift2[Function1].andThen(result.is.flip)
      }
    }
}

trait ArrowTypeDeserialization2LPI {

  implicit def ArrowTypeCaseNoneGen2[T[_]: ArrowType, Ca[T[_], _]] =
    new ArrowType[λ[A => (T[A], Option[Ca[T, A]])]] {
      def tarrow[_T1, _T2](
          ot1: (T[_T1], Option[Ca[T, _T1]]),
          ot2: (T[_T2], Option[Ca[T, _T2]])
      ) =
        (ArrowType[T].tarrow(ot1._1, ot2._1), None)
    }

  implicit def ArrowTypeCaseNoneGen1[Ca[_]] =
    new ArrowType[λ[A => Option[Ca[A]]]] {
      def tarrow[_T1, _T2](
          ot1: Option[Ca[_T1]],
          ot2: Option[Ca[_T2]]
      ) = None
    }
}

trait ArrowTypeSerialization {

  import trees._, TreeSerializable.ShowTree

  trait Constructors {
    def tr_tArr(t1: Tree, t2: Tree): Tree =
      Node("TArr", List(t1, t2))
  }

  object Constructors extends Constructors

  implicit def _ShowTree = new ArrowType[ShowTree] {
    def tarrow[T1, T2](t1: ShowTree[T1], t2: ShowTree[T2]): ShowTree[T1 => T2] =
      (i: Int) => Constructors.tr_tArr(t1(i), t2(i))
  }
}

trait ArrowTypeLPI {

  implicit val _ShowP = new ArrowType[ShowP] {
    def tarrow[T1, T2](t1: String, t2: String): String =
      s"$t1 -> $t2"
  }
}

trait ArrowTypeSyntax {

  def tarrow[T[_], T1, T2](t1: T[T1], t2: T[T2])(implicit T: ArrowType[T]): T[T1 => T2] =
    T.tarrow(t1, t2)
}
