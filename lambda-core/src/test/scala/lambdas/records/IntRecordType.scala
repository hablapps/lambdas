package lambdas
package records

import semantics.Record
import shapeless.HList
import cats.~>
import arithmetic.{ IntType, Num }

trait IntRecordType[A] {
  def apply[T[_]](implicit I: IntType[T], R: RecordType[T], A: ArrowType[T]): T[A]
}

object IntRecordType
    extends IntRecordTypeInstances
    with IntRecordTypeDeserialization
    with IntRecordTypeLPI
    with IntType.Implicits[IntRecordType]
    with RecordType.Implicits[IntRecordType]
    with ArrowType.Implicits[IntRecordType]
    with IntRecordTypeSerialization

trait IntRecordTypeInstances {

  implicit val _IntType = new IntType[IntRecordType] {
    def tint: IntRecordType[Num] = new IntRecordType[Num] {
      def apply[T[_]](implicit I: IntType[T], R: RecordType[T], A: ArrowType[T]): T[Num] =
        I.tint
    }
  }

  implicit val _RecordType = new RecordType[IntRecordType] {
    def tRecord[L <: HList](f: Fields[IntRecordType, L]): IntRecordType[Record[L]] =
      new IntRecordType[Record[L]] {
        def apply[T[_]](implicit I: IntType[T], R: RecordType[T], A: ArrowType[T]): T[Record[L]] =
          R.tRecord(f(λ[IntRecordType ~> T](_(I, R, A))))
      }
  }

  implicit val _ArrowType = new ArrowType[IntRecordType] {
    def tarrow[T1, T2](
        t1: IntRecordType[T1],
        t2: IntRecordType[T2]
    ): IntRecordType[T1 => T2] = new IntRecordType[T1 => T2] {
      def apply[T[_]](implicit I: IntType[T], R: RecordType[T], A: ArrowType[T]): T[T1 => T2] =
        A.tarrow(t1(I, R, A), t2(I, R, A))
    }
  }
}

trait IntRecordTypeDeserialization {

  import safecast._
  import cats.evidence.Is

  implicit val IntTypeMatch = new IntType.Match[IntRecordType] {
    def unapply[A](t: IntRecordType[A]): Option[IntType.Case[A]] =
      t[λ[T => Option[IntType.Case[T]]]]
  }

  implicit val RecordTypeMatch = new RecordType.Match[IntRecordType] {
    def unapply[A](t: IntRecordType[A]): Option[RecordType.Case[IntRecordType, A]] =
      t[λ[T => (IntRecordType[T], Option[RecordType.Case[IntRecordType, T]])]]._2
  }

  implicit val ArrowTypeMatch = new ArrowType.Arrow1.Match[IntRecordType] {
    def unapply[A](t: IntRecordType[A]): Option[ArrowType.Arrow1.Case[IntRecordType, A]] =
      t[λ[T => (IntRecordType[T], Option[ArrowType.Arrow1.Case[IntRecordType, T]])]]._2
  }

  implicit val _Cast = new Cast[IntRecordType] {
    def apply[T1, T2](t1: IntRecordType[T1], t2: IntRecordType[T2]): Option[T1 Is T2] =
      t1[Cast.As[IntRecordType, ?]].apply(t2)
  }
}

trait IntRecordTypeSerialization {

  import trees._, TreeSerializable.ShowTree

  implicit val serializer = new TreeSerializable[IntRecordType] {
    def show[A](t: IntRecordType[A]): Tree =
      t[ShowTree].apply(0)
  }
}

trait IntRecordTypeLPI {
  implicit val _ForallShow = new Forall0[IntRecordType, cats.Show] {
    def apply[A]() = new cats.Show[IntRecordType[A]] {
      def show(t: IntRecordType[A]): String =
        t[ShowP]
    }
  }
}
