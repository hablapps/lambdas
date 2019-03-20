package lambdas
package intArrowParser

import cats.evidence.Is

import safecast._
import arithmetic.IntType
import tfdb.ArrowType

trait IntArrowTypeTerm[A] {
  def apply[T[_]](implicit I: IntType[T], A: ArrowType[T]): T[A]
}

object IntArrowTypeTerm {

  implicit val _IntType = new IntType[IntArrowTypeTerm] {
    def tint: IntArrowTypeTerm[Int] = new IntArrowTypeTerm[Int] {
      def apply[T[_]](implicit I: IntType[T], A: ArrowType[T]): T[Int] =
        I.tint
    }
  }

  implicit val _ArrowType = new ArrowType[IntArrowTypeTerm] {
    def tarrow[T1, T2](
        t1: IntArrowTypeTerm[T1],
        t2: IntArrowTypeTerm[T2]
    ): IntArrowTypeTerm[T1 => T2] = new IntArrowTypeTerm[T1 => T2] {
      def apply[T[_]](implicit I: IntType[T], A: ArrowType[T]): T[T1 => T2] =
        A.tarrow(t1(I, A), t2(I, A))
    }
  }

  implicit val IntTypeMatch = new Match[IntArrowTypeTerm, IntType.Case] {

    implicit val TIsInt_ArrowType = new ArrowType[λ[T => Option[IntType.Case[T]]]] {
      def tarrow[T1, T2](
          t1: Option[IntType.Case[T1]],
          t2: Option[IntType.Case[T2]]
      ): Option[IntType.Case[T1 => T2]] =
        None
    }

    def unapply[A](t: IntArrowTypeTerm[A]): Option[IntType.Case[A]] =
      t[λ[T => Option[IntType.Case[T]]]]
  }

  implicit val ArrowTypeMatch = new Match[IntArrowTypeTerm, ArrowType.Case[IntArrowTypeTerm, ?]] {

    implicit val TIsArrow_IntType =
      new IntType[λ[T => (IntArrowTypeTerm[T], Option[ArrowType.Case[IntArrowTypeTerm, T]])]] {
        def tint = (IntType[IntArrowTypeTerm].tint, None)
      }

    def unapply[A](t: IntArrowTypeTerm[A]): Option[ArrowType.Case[IntArrowTypeTerm, A]] =
      t[λ[T => (IntArrowTypeTerm[T], Option[ArrowType.Case[IntArrowTypeTerm, T]])]]._2
  }

  implicit val _Cast = new Cast[IntArrowTypeTerm] {
    def apply[T1, T2](t1: IntArrowTypeTerm[T1], t2: IntArrowTypeTerm[T2]): Option[T1 Is T2] =
      t1[Cast.As[IntArrowTypeTerm, ?]].apply(t2)
  }

  val _NotPatentlyTotalCast = new Cast[IntArrowTypeTerm] {
    def apply[T1, T2](t1: IntArrowTypeTerm[T1], t2: IntArrowTypeTerm[T2]): Option[T1 Is T2] =
      (t1, t2) match {

        case (IntTypeMatch(c1), IntTypeMatch(c2)) =>
          Option(c1.is andThen c2.is.flip)

        case (ArrowTypeMatch(c1), ArrowTypeMatch(c2)) =>
          for {
            t11Ist12 <- apply(c1.t1, c2.t1)
            t21Ist22 <- apply(c1.t2, c2.t2)
          } yield c1.is andThen (t11Ist12, t21Ist22).lift2[Function1] andThen c2.is.flip

        case (_, _) =>
          None
      }
  }

  implicit val _ForallShow = new ForAll0[IntArrowTypeTerm, cats.Show] {
    def apply[A]() = new cats.Show[IntArrowTypeTerm[A]] {
      def show(t: IntArrowTypeTerm[A]): String =
        t[ShowP]
    }
  }
}
