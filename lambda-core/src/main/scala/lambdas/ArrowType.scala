package lambdas

import cats.evidence._
import safecast._

trait ArrowType[T[_]] {
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

trait ArrowTypeDeserialization {

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
    implicit def Case_ArrowType[T[_]: ArrowType] =
      new ArrowType[λ[A => (T[A], Option[Case[T, A]])]] {
        def tarrow[_T1, _T2](
            ot1: (T[_T1], Option[Case[T, _T1]]),
            ot2: (T[_T2], Option[Case[T, _T2]])
        ) =
          (ArrowType[T].tarrow(ot1._1, ot2._1), Option(new Case[T, _T1 => _T2] {
            type T1 = _T1
            type T2 = _T2

            val t1 = ot1._1
            val t2 = ot2._1
            val is = Is.refl[T1 => T2]
          }))
      }
  }

  type Match[T[_]] = safecast.Match[T, Case[T, ?]]

  implicit def ArrowTypeCast[T[_]: ArrowType](implicit IsArrow: Match[T]) =
    new ArrowType[Cast.As[T, ?]] {
      def tarrow[T0, T1](t0: Cast.As[T, T0], t1: Cast.As[T, T1]) = new Cast.As[T, T0 => T1] {
        def apply[T2](t2: T[T2]): Option[(T0 => T1) Is T2] =
          for {
            result <- IsArrow.unapply(t2)
            eqT0   <- t0(result.t1)
            eqT1   <- t1(result.t2)
          } yield (eqT0, eqT1).lift2[Function1].andThen(result.is.flip)
      }
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
