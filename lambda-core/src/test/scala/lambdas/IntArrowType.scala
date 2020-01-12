package lambdas

import arithmetic.{ IntType, Num }

trait IntArrowType[A] {
  def apply[T[_]](implicit I: IntType[T], R: ArrowType[T], P: ProductType[T]): T[A]
}

object IntArrowType
    extends IntArrowTypeInstances
    with IntType.Implicits[IntArrowType]
    with ProductType.Implicits[IntArrowType]
    with ArrowType.Implicits[IntArrowType]
    with IntArrowTypeDeserialization
    with IntArrowTypeSerialization
    with IntArrowTypeLPI

trait IntArrowTypeInstances {

  implicit val _IntType = new IntType[IntArrowType] {
    def tint: IntArrowType[Num] = new IntArrowType[Num] {
      def apply[T[_]](implicit I: IntType[T], R: ArrowType[T], P: ProductType[T]): T[Num] =
        I.tint
    }
  }

  implicit val _ArrowType = new ArrowType[IntArrowType] {
    def tarrow[T1, T2](
        t1: IntArrowType[T1],
        t2: IntArrowType[T2]
    ): IntArrowType[T1 => T2] = new IntArrowType[T1 => T2] {
      def apply[T[_]](implicit I: IntType[T], R: ArrowType[T], P: ProductType[T]): T[T1 => T2] =
        R.tarrow(t1(I, R, P), t2(I, R, P))
    }
  }

  implicit val _ProductType = new ProductType[IntArrowType] {
    def tProduct[T1, T2](
        t1: IntArrowType[T1],
        t2: IntArrowType[T2]
    ): IntArrowType[(T1, T2)] = new IntArrowType[(T1, T2)] {
      def apply[T[_]](implicit I: IntType[T], R: ArrowType[T], P: ProductType[T]): T[(T1, T2)] =
        P.tProduct(t1(I, R, P), t2(I, R, P))
    }
  }
}

trait IntArrowTypeDeserialization {

  implicit val IntTypeMatch = new IntType.Match[IntArrowType] {
    def unapply[A](t: IntArrowType[A]): Option[IntType.Case[A]] =
      t[λ[T => Option[IntType.Case[T]]]]
  }

  implicit val ArrowTypeMatch = new ArrowType.Arrow1.Match[IntArrowType] {
    def unapply[A](t: IntArrowType[A]): Option[ArrowType.Arrow1.Case[IntArrowType, A]] =
      t[λ[T => (IntArrowType[T], Option[ArrowType.Arrow1.Case[IntArrowType, T]])]]._2
  }

  implicit val ProductTypeMatch = new ProductType.Match[IntArrowType] {
    def unapply[A](t: IntArrowType[A]): Option[ProductType.Case[IntArrowType, A]] =
      t[λ[T => (IntArrowType[T], Option[ProductType.Case[IntArrowType, T]])]]._2
  }

  import safecast._
  import cats.evidence.Is

  implicit val _Cast = new Cast[IntArrowType] {
    def apply[T1, T2](t1: IntArrowType[T1], t2: IntArrowType[T2]): Option[T1 Is T2] =
      t1[Cast.As[IntArrowType, ?]].apply(t2)
  }

  import scala.language.postfixOps
  import cats.instances.string._
  import interpreters._
  import trees._, arithparser._, tfdbparser._

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
