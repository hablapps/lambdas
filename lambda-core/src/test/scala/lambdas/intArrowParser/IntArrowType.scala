package lambdas
package intArrowParser

import cats.evidence.Is
import cats.instances.string._

import arithmetic.semantics._
import arithmetic.IntType
import tfdb.semantics._
import tfdb.ArrowType

import trees._
import interpreters._
import safecast._

trait IntArrowType[A] {
  def apply[T[_]](implicit I: IntType[T], A: ArrowType[T]): T[A]
}

object IntArrowType {

  implicit val _IntType = new IntType[IntArrowType] {
    def tint: IntArrowType[Int] = new IntArrowType[Int] {
      def apply[T[_]](implicit I: IntType[T], A: ArrowType[T]): T[Int] =
        I.tint
    }
  }

  implicit val _ArrowType = new ArrowType[IntArrowType] {
    def tarrow[T1, T2](
        t1: IntArrowType[T1],
        t2: IntArrowType[T2]
    ): IntArrowType[T1 => T2] = new IntArrowType[T1 => T2] {
      def apply[T[_]](implicit I: IntType[T], A: ArrowType[T]): T[T1 => T2] =
        A.tarrow(t1(I, A), t2(I, A))
    }
  }

  implicit val IntTypeMatch = new IntType.Match[IntArrowType] {

    implicit val TIsInt_ArrowType = new ArrowType[λ[T => Option[IntType.Case[T]]]] {
      def tarrow[T1, T2](
          t1: Option[IntType.Case[T1]],
          t2: Option[IntType.Case[T2]]
      ): Option[IntType.Case[T1 => T2]] =
        None
    }

    def unapply[A](t: IntArrowType[A]): Option[IntType.Case[A]] =
      t[λ[T => Option[IntType.Case[T]]]]
  }

  implicit val ArrowTypeMatch = new ArrowType.Match[IntArrowType] {

    implicit val TIsArrow_IntType =
      new IntType[λ[T => (IntArrowType[T], Option[ArrowType.Case[IntArrowType, T]])]] {
        def tint = (IntType[IntArrowType].tint, None)
      }

    def unapply[A](t: IntArrowType[A]): Option[ArrowType.Case[IntArrowType, A]] =
      t[λ[T => (IntArrowType[T], Option[ArrowType.Case[IntArrowType, T]])]]._2
  }

  implicit val _Cast = new Cast[IntArrowType] {
    def apply[T1, T2](t1: IntArrowType[T1], t2: IntArrowType[T2]): Option[T1 Is T2] =
      t1[Cast.As[IntArrowType, ?]].apply(t2)
  }

  val _NotPatentlyTotalCast = new Cast[IntArrowType] {
    def apply[T1, T2](t1: IntArrowType[T1], t2: IntArrowType[T2]): Option[T1 Is T2] =
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

  implicit val _ForallShow = new ForAll0[IntArrowType, cats.Show] {
    def apply[A]() = new cats.Show[IntArrowType[A]] {
      def show(t: IntArrowType[A]): String =
        t[ShowP]
    }
  }

  val parser: Interpreter[Tree, Either[String, ATypeTerm[IntArrowType]]] =
    ArrowTypeParser[IntArrowType] orElse
    IntTypeParser[IntArrowType] close
}
