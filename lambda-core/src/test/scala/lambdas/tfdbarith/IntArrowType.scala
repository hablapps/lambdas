package lambdas
package tfdbarith

import arithmetic.IntType

trait IntArrowType[A] {
  def apply[T[_]](implicit I: IntType[T], R: ArrowType[T]): T[A]
}

object IntArrowType
    extends IntArrowTypeInstances
    with IntType.Implicits[IntArrowType]
    with ArrowType.Implicits[IntArrowType]
    with IntArrowTypeDeserialization
    with IntArrowTypeSerialization
    with IntArrowTypeLPI

trait IntArrowTypeInstances {

  implicit val _IntType = new IntType[IntArrowType] {
    def tint: IntArrowType[Int] = new IntArrowType[Int] {
      def apply[T[_]](implicit I: IntType[T], R: ArrowType[T]): T[Int] =
        I.tint
    }
  }

  implicit val _ArrowType = new ArrowType[IntArrowType] {
    def tarrow[T1, T2](
        t1: IntArrowType[T1],
        t2: IntArrowType[T2]
    ): IntArrowType[T1 => T2] = new IntArrowType[T1 => T2] {
      def apply[T[_]](implicit I: IntType[T], R: ArrowType[T]): T[T1 => T2] =
        R.tarrow(t1(I, R), t2(I, R))
    }

    def tarrow2[T1, T2, T3](
        t1: IntArrowType[T1],
        t2: IntArrowType[T2],
        t3: IntArrowType[T3]
    ): IntArrowType[(T1, T2) => T3] = new IntArrowType[(T1, T2) => T3] {
      def apply[T[_]](implicit I: IntType[T], R: ArrowType[T]): T[(T1, T2) => T3] =
        R.tarrow2(t1(I, R), t2(I, R), t3(I, R))
    }
  }
}

trait IntArrowTypeDeserialization {
  import scala.language.postfixOps
  import cats.evidence.Is
  import cats.instances.string._
  import safecast._
  import interpreters._
  import trees._, arithparser._, tfdbparser._

  implicit val _IntTypeMatch = new IntType.Match[IntArrowType] {
    def unapply[A](t: IntArrowType[A]): Option[IntType.Case[A]] =
      t[λ[T => Option[IntType.Case[T]]]]
  }

  implicit val _ArrowTypeMatch = new ArrowType.Match[IntArrowType] {
    def unapply[A](t: IntArrowType[A]): Option[ArrowType.Case[IntArrowType, A]] =
      t[λ[T => (IntArrowType[T], Option[ArrowType.Case[IntArrowType, T]])]]._2
  }

  implicit val _Cast = new Cast[IntArrowType] {
    def apply[T1, T2](t1: IntArrowType[T1], t2: IntArrowType[T2]): Option[T1 Is T2] =
      t1[Cast.As[IntArrowType, ?]].apply(t2)
  }

  val parser: Interpreter[Tree, Either[String, ATypeTerm[IntArrowType]]] =
    ArrowTypeParser[IntArrowType] orElse
    IntTypeParser[IntArrowType] close
}

trait IntArrowTypeSerialization {

  import trees._, TreeSerializable.ShowTree

  implicit val serializer = new TreeSerializable[IntArrowType] {
    def show[A](t: IntArrowType[A]): Tree =
      t[ShowTree].apply(0)
  }
}

trait IntArrowTypeLPI {

  implicit val _ForallShow = new Forall0[IntArrowType, cats.Show] {
    def apply[A]() = new cats.Show[IntArrowType[A]] {
      def show(t: IntArrowType[A]): String =
        t[ShowP]
    }
  }
}
