package lambdas
package safecast

import cats.evidence._

abstract class Cast[A] {
  def apply[B](tb: TypeTerm[B]): Option[A Is B]

  def as[B, F[_]](tb: TypeTerm[B], fa: F[A]): Option[F[B]] =
    apply(tb).map(_.substitute[F](fa))
}

object Cast {

  implicit val CastType = new Type[Cast] {

    def tint = new Cast[Int] {
      def apply[B](tb: TypeTerm[B]) =
        tb[CastInt].eq map (_.flip)
    }

    def tarr[T1, T2](t1: Cast[T1], t2: Cast[T2]) = new Cast[T1 => T2] {
      def apply[B](tb: TypeTerm[B]) = {
        val asArr = tb[CastArrow]
        for {
          value <- asArr.eq
          eqT1  <- t1(value._1)
          eqT2  <- t2(value._2)
        } yield (eqT1, eqT2).lift2[Function1].andThen(value._3.flip)
      }
    }
  }
}

trait CastTC[T[_], T1] {
  def apply[T2](t2: T[T2]): Option[T1 Is T2]

  def as[T2, F[_]](t2: T[T2])(f1: F[T1]): Option[F[T2]] =
    apply(t2) map (_.substitute[F](f1))
}

trait IntType[T[_]] {
  def tint: T[Int]
}

object IntType {

  trait Match[T[_]] {
    type Info[A] = Option[A Is Int]

    def unapply[A](t: T[A]): Info[A]
  }

  def IntTypeCastTC[T[_]](implicit IsInt: Match[T]) = new CastTC[T, Int] {
    def apply[T2](t2: T[T2]): Option[Int Is T2] =
      IsInt.unapply(t2) map (_.flip)
  }
}

trait ArrowType[T[_]] {
  def tarrow[T1, T2](t1: T[T1], t2: T[T2]): T[T1 => T2]
}

object ArrowType {
  import scala.language.reflectiveCalls

  trait Match[T[_]] {
    type Info[A] = { type T1; type T2; val result: Option[(T[T1], T[T2], A Is (T1 => T2))] }

    def unapply[A](t: T[A]): Info[A]
  }

  implicit def ArrowTypeCastTC[T[_], T0, T1](
      implicit
      C1: CastTC[T, T0],
      C2: CastTC[T, T1],
      IsArrow: Match[T]
  ) =
    new CastTC[T, T0 => T1] {
      def apply[T2](t2: T[T2]): Option[(T0 => T1) Is T2] = {
        val value = IsArrow.unapply(t2)
        for {
          result <- value.result
          eqT1   <- C1(result._1)
          eqT2   <- C2(result._2)
        } yield (eqT1, eqT2).lift2[Function1].andThen(result._3.flip)
      }
    }
}

trait IntArrowTypeTerm[A] {
  def apply[T[_]](implicit I: IntType[T], A: ArrowType[T]): T[A]
}

object IntArrowTypeTerm {

  val IntArrowTypeTerm_IntType = new IntType[IntArrowTypeTerm] {
    def tint: IntArrowTypeTerm[Int] = new IntArrowTypeTerm[Int] {
      def apply[T[_]](implicit I: IntType[T], A: ArrowType[T]): T[Int] =
        I.tint
    }
  }

  val IntArrowTypeTerm_ArrowType = new ArrowType[IntArrowTypeTerm] {
    def tarrow[T1, T2](
        t1: IntArrowTypeTerm[T1],
        t2: IntArrowTypeTerm[T2]
    ): IntArrowTypeTerm[T1 => T2] = new IntArrowTypeTerm[T1 => T2] {
      def apply[T[_]](implicit I: IntType[T], A: ArrowType[T]): T[T1 => T2] =
        A.tarrow(t1(I, A), t2(I, A))
    }
  }

  val IntArrowTypeTerm_IntTypeMatch = new IntType.Match[IntArrowTypeTerm] {

    val TIsInt_IntType = new IntType[IntType.Match[IntArrowTypeTerm]#Info] {
      def tint: Option[Int Is Int] =
        Option(Is.refl[Int])
    }

    val TIsInt_ArrowType = new ArrowType[IntType.Match[IntArrowTypeTerm]#Info] {
      def tarrow[T1, T2](t1: Option[T1 Is Int], t2: Option[T2 Is Int]): Option[(T1 => T2) Is Int] =
        None
    }

    def unapply[A](t: IntArrowTypeTerm[A]): Option[A Is Int] =
      t[λ[T => Option[T Is Int]]](TIsInt_IntType, TIsInt_ArrowType)
  }

  val IntArrowTypeTerm_ArrowTypeMatch = new ArrowType.Match[IntArrowTypeTerm] {

    val TIsArrow_IntType =
      new IntType[λ[A => (IntArrowTypeTerm[A], ArrowType.Match[IntArrowTypeTerm]#Info[A])]] {
        def tint =
          (IntArrowTypeTerm_IntType.tint, new {
            type T1 = Nothing; type T2 = Nothing; val result = None
          })
      }

    val TIsArrow_ArrowType =
      new ArrowType[λ[A => (IntArrowTypeTerm[A], ArrowType.Match[IntArrowTypeTerm]#Info[A])]] {
        def tarrow[_T1, _T2](
            t1: (IntArrowTypeTerm[_T1], ArrowType.Match[IntArrowTypeTerm]#Info[_T1]),
            t2: (IntArrowTypeTerm[_T2], ArrowType.Match[IntArrowTypeTerm]#Info[_T2])
        ) =
          (IntArrowTypeTerm_ArrowType.tarrow(t1._1, t2._1), new {
            type T1 = _T1
            type T2 = _T2
            val result = Some(t1._1, t2._1, Is.refl[T1 => T2])
          })
      }

    def unapply[A](t: IntArrowTypeTerm[A]): {
      type T1;
      type T2;
      val result: Option[(IntArrowTypeTerm[T1], IntArrowTypeTerm[T2], A Is (T1 => T2))]
    } =
      t[λ[A => (IntArrowTypeTerm[A], ArrowType.Match[IntArrowTypeTerm]#Info[A])]](
        TIsArrow_IntType,
        TIsArrow_ArrowType
      )._2
  }
}
